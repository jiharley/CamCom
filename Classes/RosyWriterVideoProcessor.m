/*
     File: RosyWriterVideoProcessor.m
 Abstract: The class that creates and manages the AV capture session and asset writer
  Version: 1.2
 
 Disclaimer: IMPORTANT:  This Apple software is supplied to you by Apple
 Inc. ("Apple") in consideration of your agreement to the following
 terms, and your use, installation, modification or redistribution of
 this Apple software constitutes acceptance of these terms.  If you do
 not agree with these terms, please do not use, install, modify or
 redistribute this Apple software.
 
 In consideration of your agreement to abide by the following terms, and
 subject to these terms, Apple grants you a personal, non-exclusive
 license, under Apple's copyrights in this original Apple software (the
 "Apple Software"), to use, reproduce, modify and redistribute the Apple
 Software, with or without modifications, in source and/or binary forms;
 provided that if you redistribute the Apple Software in its entirety and
 without modifications, you must retain this notice and the following
 text and disclaimers in all such redistributions of the Apple Software.
 Neither the name, trademarks, service marks or logos of Apple Inc. may
 be used to endorse or promote products derived from the Apple Software
 without specific prior written permission from Apple.  Except as
 expressly stated in this notice, no other rights or licenses, express or
 implied, are granted by Apple herein, including but not limited to any
 patent rights that may be infringed by your derivative works or by other
 works in which the Apple Software may be incorporated.
 
 The Apple Software is provided by Apple on an "AS IS" basis.  APPLE
 MAKES NO WARRANTIES, EXPRESS OR IMPLIED, INCLUDING WITHOUT LIMITATION
 THE IMPLIED WARRANTIES OF NON-INFRINGEMENT, MERCHANTABILITY AND FITNESS
 FOR A PARTICULAR PURPOSE, REGARDING THE APPLE SOFTWARE OR ITS USE AND
 OPERATION ALONE OR IN COMBINATION WITH YOUR PRODUCTS.
 
 IN NO EVENT SHALL APPLE BE LIABLE FOR ANY SPECIAL, INDIRECT, INCIDENTAL
 OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
 SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
 INTERRUPTION) ARISING IN ANY WAY OUT OF THE USE, REPRODUCTION,
 MODIFICATION AND/OR DISTRIBUTION OF THE APPLE SOFTWARE, HOWEVER CAUSED
 AND WHETHER UNDER THEORY OF CONTRACT, TORT (INCLUDING NEGLIGENCE),
 STRICT LIABILITY OR OTHERWISE, EVEN IF APPLE HAS BEEN ADVISED OF THE
 POSSIBILITY OF SUCH DAMAGE.
 
 Copyright (C) 2011 Apple Inc. All Rights Reserved.
 
 */

#import <MobileCoreServices/MobileCoreServices.h>
#import <AssetsLibrary/AssetsLibrary.h>
#import "RosyWriterVideoProcessor.h"

#define DATA 1
#define SFD 0
#define DataWillBegin 0
#define SFDWillEnd 0
#define DataJustBegin 1
#define SFDJustBegin 2
#define DataDidBegin 3
#define BYTES_PER_PIXEL 4
#define SamplePixelNumbers 24

#define START_ROW 190
#define END_ROW 350
#define START_COLUMN 400
#define END_COLUMN 560

#define MIDDLE_ROW 270
#define MIDDLE_COLUMN 480
#define SampleLineLength 240
#define HalfSampleLineLength 120

#define HALF_SQUARE_LENGTH 60
#define SQUARE_LENGTH 120

typedef struct Pixel{
    short x , y;
}Pixel;

typedef struct columnPixel {
    int array[3][240];
}ColumnPixel;
typedef struct samplePixelArray {
    int array[SamplePixelNumbers];
}SamplePixelArray;
@interface RosyWriterVideoProcessor ()

// Redeclared as readwrite so that we can write to the property and still be atomic with external readers.
@property (readwrite) Float64 videoFrameRate;
@property (readwrite) CMVideoDimensions videoDimensions;
@property (readwrite) CMVideoCodecType videoType;

@property (readwrite, getter=isRecording) BOOL recording;

@property (readwrite) AVCaptureVideoOrientation videoOrientation;

@end

@implementation RosyWriterVideoProcessor

@synthesize delegate;
@synthesize videoFrameRate, videoDimensions, videoType;
@synthesize referenceOrientation;
@synthesize videoOrientation;
@synthesize recording;

- (id) init
{
    if (self = [super init]) {
        previousSecondTimestamps = [[NSMutableArray alloc] init];
        referenceOrientation = UIDeviceOrientationPortrait;
        
        lastFrame = DATA;
        lastState = DataDidBegin;
        startStateMachine = false;
        cntDataFrame = 0;
        cntTraining = 0;
        dataFrameBuffer = [[NSMutableArray alloc] init];
        trainingPixelValue = 0;
        trainingFinished = false;
//        demodulate_queue = dispatch_queue_create("demodulate_queue", nil);
                
        // The temporary path for the video before saving it to the photo album
        movieURL = [NSURL fileURLWithPath:[NSString stringWithFormat:@"%@%@", NSTemporaryDirectory(), @"Movie.MOV"]];
        [movieURL retain];
    }
    return self;
}

- (void)dealloc 
{
    [previousSecondTimestamps release];
    [movieURL release];
    
    if (demodulate_queue) {
        dispatch_release(demodulate_queue);
        demodulate_queue = NULL;
    }
    [dataFrameBuffer release];
	[super dealloc];
}

#pragma mark Utilities

- (void) calculateFramerateAtTimestamp:(CMTime) timestamp
{
	[previousSecondTimestamps addObject:[NSValue valueWithCMTime:timestamp]];
    
	CMTime oneSecond = CMTimeMake( 1, 1 );
	CMTime oneSecondAgo = CMTimeSubtract( timestamp, oneSecond );
    
	while( CMTIME_COMPARE_INLINE( [[previousSecondTimestamps objectAtIndex:0] CMTimeValue], <, oneSecondAgo ) )
		[previousSecondTimestamps removeObjectAtIndex:0];
    
	Float64 newRate = (Float64) [previousSecondTimestamps count];
	self.videoFrameRate = (self.videoFrameRate + newRate) / 2;
}

- (void)removeFile:(NSURL *)fileURL
{
    NSFileManager *fileManager = [NSFileManager defaultManager];
    NSString *filePath = [fileURL path];
    if ([fileManager fileExistsAtPath:filePath]) {
        NSError *error;
        BOOL success = [fileManager removeItemAtPath:filePath error:&error];
		if (!success)
			[self showError:error];
    }
}

- (CGFloat)angleOffsetFromPortraitOrientationToOrientation:(AVCaptureVideoOrientation)orientation
{
	CGFloat angle = 0.0;
	
	switch (orientation) {
		case AVCaptureVideoOrientationPortrait:
			angle = 0.0;
			break;
		case AVCaptureVideoOrientationPortraitUpsideDown:
			angle = M_PI;
			break;
		case AVCaptureVideoOrientationLandscapeRight:
			angle = -M_PI_2;
			break;
		case AVCaptureVideoOrientationLandscapeLeft:
			angle = M_PI_2;
			break;
		default:
			break;
	}

	return angle;
}

- (CGAffineTransform)transformFromCurrentVideoOrientationToOrientation:(AVCaptureVideoOrientation)orientation
{
	CGAffineTransform transform = CGAffineTransformIdentity;

	// Calculate offsets from an arbitrary reference orientation (portrait)
	CGFloat orientationAngleOffset = [self angleOffsetFromPortraitOrientationToOrientation:orientation];
	CGFloat videoOrientationAngleOffset = [self angleOffsetFromPortraitOrientationToOrientation:self.videoOrientation];
	
	// Find the difference in angle between the passed in orientation and the current video orientation
	CGFloat angleOffset = orientationAngleOffset - videoOrientationAngleOffset;
	transform = CGAffineTransformMakeRotation(angleOffset);
	
	return transform;
}

#pragma mark Recording

- (void)saveMovieToCameraRoll
{
	ALAssetsLibrary *library = [[ALAssetsLibrary alloc] init];
	[library writeVideoAtPathToSavedPhotosAlbum:movieURL
								completionBlock:^(NSURL *assetURL, NSError *error) {
									if (error)
										[self showError:error];
									else
										[self removeFile:movieURL];
									
									dispatch_async(movieWritingQueue, ^{
										recordingWillBeStopped = NO;
										self.recording = NO;
										
										[self.delegate recordingDidStop];
									});
								}];
	[library release];
}

- (void) writeSampleBuffer:(CMSampleBufferRef)sampleBuffer ofType:(NSString *)mediaType
{
	if ( assetWriter.status == AVAssetWriterStatusUnknown ) {
		
        if ([assetWriter startWriting]) {			
			[assetWriter startSessionAtSourceTime:CMSampleBufferGetPresentationTimeStamp(sampleBuffer)];
		}
		else {
			[self showError:[assetWriter error]];
		}
	}
	
	if ( assetWriter.status == AVAssetWriterStatusWriting ) {
		
		if (mediaType == AVMediaTypeVideo) {
			if (assetWriterVideoIn.readyForMoreMediaData) {
				if (![assetWriterVideoIn appendSampleBuffer:sampleBuffer]) {
					[self showError:[assetWriter error]];
				}
			}
		}
		else if (mediaType == AVMediaTypeAudio) {
			if (assetWriterAudioIn.readyForMoreMediaData) {
				if (![assetWriterAudioIn appendSampleBuffer:sampleBuffer]) {
					[self showError:[assetWriter error]];
				}
			}
		}
	}
}

- (BOOL) setupAssetWriterAudioInput:(CMFormatDescriptionRef)currentFormatDescription
{
	const AudioStreamBasicDescription *currentASBD = CMAudioFormatDescriptionGetStreamBasicDescription(currentFormatDescription);

	size_t aclSize = 0;
	const AudioChannelLayout *currentChannelLayout = CMAudioFormatDescriptionGetChannelLayout(currentFormatDescription, &aclSize);
	NSData *currentChannelLayoutData = nil;
	
	// AVChannelLayoutKey must be specified, but if we don't know any better give an empty data and let AVAssetWriter decide.
	if ( currentChannelLayout && aclSize > 0 )
		currentChannelLayoutData = [NSData dataWithBytes:currentChannelLayout length:aclSize];
	else
		currentChannelLayoutData = [NSData data];
	
	NSDictionary *audioCompressionSettings = [NSDictionary dictionaryWithObjectsAndKeys:
											  [NSNumber numberWithInteger:kAudioFormatMPEG4AAC], AVFormatIDKey,
											  [NSNumber numberWithFloat:currentASBD->mSampleRate], AVSampleRateKey,
											  [NSNumber numberWithInt:64000], AVEncoderBitRatePerChannelKey,
											  [NSNumber numberWithInteger:currentASBD->mChannelsPerFrame], AVNumberOfChannelsKey,
											  currentChannelLayoutData, AVChannelLayoutKey,
											  nil];
	if ([assetWriter canApplyOutputSettings:audioCompressionSettings forMediaType:AVMediaTypeAudio]) {
		assetWriterAudioIn = [[AVAssetWriterInput alloc] initWithMediaType:AVMediaTypeAudio outputSettings:audioCompressionSettings];
		assetWriterAudioIn.expectsMediaDataInRealTime = YES;
		if ([assetWriter canAddInput:assetWriterAudioIn])
			[assetWriter addInput:assetWriterAudioIn];
		else {
			NSLog(@"Couldn't add asset writer audio input.");
            return NO;
		}
	}
	else {
		NSLog(@"Couldn't apply audio output settings.");
        return NO;
	}
    
    return YES;
}

- (BOOL) setupAssetWriterVideoInput:(CMFormatDescriptionRef)currentFormatDescription 
{
	float bitsPerPixel;
	CMVideoDimensions dimensions = CMVideoFormatDescriptionGetDimensions(currentFormatDescription);
	int numPixels = dimensions.width * dimensions.height;
	int bitsPerSecond;
	
	// Assume that lower-than-SD resolutions are intended for streaming, and use a lower bitrate
	if ( numPixels < (640 * 480) )
		bitsPerPixel = 4.05; // This bitrate matches the quality produced by AVCaptureSessionPresetMedium or Low.
	else
		bitsPerPixel = 11.4; // This bitrate matches the quality produced by AVCaptureSessionPresetHigh.
	
	bitsPerSecond = numPixels * bitsPerPixel;
	
	NSDictionary *videoCompressionSettings = [NSDictionary dictionaryWithObjectsAndKeys:
											  AVVideoCodecH264, AVVideoCodecKey,
											  [NSNumber numberWithInteger:dimensions.width], AVVideoWidthKey,
											  [NSNumber numberWithInteger:dimensions.height], AVVideoHeightKey,
											  [NSDictionary dictionaryWithObjectsAndKeys:
											   [NSNumber numberWithInteger:bitsPerSecond], AVVideoAverageBitRateKey,
											   [NSNumber numberWithInteger:30], AVVideoMaxKeyFrameIntervalKey,
											   nil], AVVideoCompressionPropertiesKey,
											  nil];
	if ([assetWriter canApplyOutputSettings:videoCompressionSettings forMediaType:AVMediaTypeVideo]) {
		assetWriterVideoIn = [[AVAssetWriterInput alloc] initWithMediaType:AVMediaTypeVideo outputSettings:videoCompressionSettings];
		assetWriterVideoIn.expectsMediaDataInRealTime = YES;
		assetWriterVideoIn.transform = [self transformFromCurrentVideoOrientationToOrientation:self.referenceOrientation];
		if ([assetWriter canAddInput:assetWriterVideoIn])
			[assetWriter addInput:assetWriterVideoIn];
		else {
			NSLog(@"Couldn't add asset writer video input.");
            return NO;
		}
	}
	else {
		NSLog(@"Couldn't apply video output settings.");
        return NO;
	}
    
    return YES;
}

- (void) startRecording
{
	dispatch_async(movieWritingQueue, ^{
	
		if ( recordingWillBeStarted || self.recording )
			return;

		recordingWillBeStarted = YES;

		// recordingDidStart is called from captureOutput:didOutputSampleBuffer:fromConnection: once the asset writer is setup
		[self.delegate recordingWillStart];

		// Remove the file if one with the same name already exists
		[self removeFile:movieURL];
			
		// Create an asset writer
		NSError *error;
		assetWriter = [[AVAssetWriter alloc] initWithURL:movieURL fileType:(NSString *)kUTTypeQuickTimeMovie error:&error];
		if (error)
			[self showError:error];
	});	
}

- (void) stopRecording
{
	dispatch_async(movieWritingQueue, ^{
		
		if ( recordingWillBeStopped || (self.recording == NO) )
			return;
		
		recordingWillBeStopped = YES;
		
		// recordingDidStop is called from saveMovieToCameraRoll
		[self.delegate recordingWillStop];

		if ([assetWriter finishWriting]) {
			[assetWriterAudioIn release];
			[assetWriterVideoIn release];
			[assetWriter release];
			assetWriter = nil;
			
			readyToRecordVideo = NO;
			readyToRecordAudio = NO;
			
			[self saveMovieToCameraRoll];
		}
		else {
			[self showError:[assetWriter error]];
		}
	});
}

- (void) startDemodulate {
    startingDemodulate = YES;
}
- (void) stopDemodulate {
    startingDemodulate = NO;
}
#pragma mark Processing

- (void)processPixelBuffer: (CVImageBufferRef)pixelBuffer 
{
	CVPixelBufferLockBaseAddress( pixelBuffer, 0 );
	
	int bufferWidth = CVPixelBufferGetWidth(pixelBuffer);
//	int bufferHeight = CVPixelBufferGetHeight(pixelBuffer);
	unsigned char *pixel = (unsigned char *)CVPixelBufferGetBaseAddress(pixelBuffer);
    
    //three sample lines
    for (int row = MIDDLE_ROW - HalfSampleLineLength; row<MIDDLE_ROW+HalfSampleLineLength; row++) {
        for (int column = MIDDLE_COLUMN -2; column < MIDDLE_COLUMN; column++) {
            unsigned char *tmpPixel  = row * bufferWidth * BYTES_PER_PIXEL + column *BYTES_PER_PIXEL + pixel;
            tmpPixel[0] = tmpPixel[1] = tmpPixel[2] = 255;
        }
        for (int column = MIDDLE_COLUMN -42; column < MIDDLE_COLUMN-40; column++) {
                unsigned char *tmpPixel  = row * bufferWidth * BYTES_PER_PIXEL + column *BYTES_PER_PIXEL + pixel;
                tmpPixel[0] = tmpPixel[1] = tmpPixel[2] = 255;
        }  
        for (int column = MIDDLE_COLUMN +38; column < MIDDLE_COLUMN+40; column++) {
            unsigned char *tmpPixel  = row * bufferWidth * BYTES_PER_PIXEL + column *BYTES_PER_PIXEL + pixel;
            tmpPixel[0] = tmpPixel[1] = tmpPixel[2] = 255;
        }
    }
/*
    //sceneview window horizontal border line
    for (int row = START_ROW - 2; row < START_ROW; row++) {
        for (int column = START_COLUMN - 2; column < END_COLUMN + 2; column ++) {
            unsigned char *tmpPixel  = row * bufferWidth * BYTES_PER_PIXEL + column *BYTES_PER_PIXEL + pixel;
            tmpPixel[0] = tmpPixel[1] = tmpPixel[2] = 255;
        }
    }
    for (int row = END_ROW; row < END_ROW + 2; row++) {
        for (int column = START_COLUMN - 2; column < END_COLUMN + 2; column ++) {
            unsigned char *tmpPixel  = row * bufferWidth * BYTES_PER_PIXEL + column *BYTES_PER_PIXEL + pixel;
            tmpPixel[0] = tmpPixel[1] = tmpPixel[2] = 255;
        }
    }
    //sceneview window vertical border line
    for (int row = START_ROW + 1; row < END_ROW; row ++) {
        for (int column = START_COLUMN - 2; column < START_COLUMN; column ++) {
            unsigned char *tmpPixel  = row * bufferWidth * BYTES_PER_PIXEL + column *BYTES_PER_PIXEL + pixel;
            tmpPixel[0] = tmpPixel[1] = tmpPixel[2] = 255;
        }
    }
    for (int row = START_ROW + 1; row < END_ROW; row ++) {
        for (int column = END_COLUMN; column < END_COLUMN + 2; column ++) {
            unsigned char *tmpPixel  = row * bufferWidth * BYTES_PER_PIXEL + column *BYTES_PER_PIXEL + pixel;
            tmpPixel[0] = tmpPixel[1] = tmpPixel[2] = 255;
        }
    }
    //sample pixel line - Red
    for (int row = MIDDLE_ROW-HalfSampleLineLength; row<MIDDLE_ROW+HalfSampleLineLength; row++) {
        for (int column=bufferWidth/2-3; column<bufferWidth/2; column++) {
            unsigned char *thisPixel = row * bufferWidth * BYTES_PER_PIXEL + column * BYTES_PER_PIXEL + pixel;
            thisPixel[2]=255;thisPixel[0]=thisPixel[1]=0;
        }
    }
*/    
    //sceneview window
    if (startingDemodulate)
    {
        SamplePixelArray pixelArr;
        int sampleCnt = 0;
        for (int row = MIDDLE_ROW-HalfSampleLineLength; row < MIDDLE_ROW+HalfSampleLineLength; row=row+10) {
            unsigned char *thisPixel = row * bufferWidth * BYTES_PER_PIXEL + MIDDLE_COLUMN * BYTES_PER_PIXEL + pixel;
//            brightness.array[sampleCnt] = (short) (3 * thisPixel[2] + 6 * thisPixel[1] + 1 * thisPixel[0])/10;
            pixelArr.array[sampleCnt] = thisPixel[2];
            sampleCnt++;
        }
        //ascend sort the sampled array
        int sortArr[SamplePixelNumbers];
        for (int i = 0; i<SamplePixelNumbers; i++) {
            sortArr[i] = pixelArr.array[i];
        }
        int k,temp;
        for (int i = 0; i<SamplePixelNumbers - 1; i++) {
            k=i;
            for (int j=i+1; j<SamplePixelNumbers; j++) {
                if (sortArr[k]>sortArr[j]){
                    k=j;
                }
            }
            if (i != k) {
                temp=sortArr[i];
                sortArr[i]=sortArr[k];
                sortArr[k]=temp;
            }
        }
        int averMin = (sortArr[0]+sortArr[1]+sortArr[2])/3;
        //before demodulating, training to get the average pixel value of dark
        if (!trainingFinished) {
            if (cntTraining < 10) {
                if (cntTraining == 0) {
                    trainingPixelValue = averMin;
                }
                else {
                    if (trainingPixelValue - averMin > 20) {
                        trainingPixelValue = averMin;
                    }
                    else if (averMin > trainingPixelValue && averMin-trainingPixelValue<20) {
                        trainingPixelValue = averMin;
                    }
                }
                cntTraining++;
            }
            else {
                trainingPixelValue = trainingPixelValue + 10;
                trainingFinished = true;
                NSLog(@"training value: %d", trainingPixelValue);
            }
        }
        else {
            if (averMin > trainingPixelValue) {
                thisFrame = SFD;
//                NSLog(@"SFD");
                startStateMachine = true;
            }
            else {
                thisFrame = DATA;
            }
        }
        
//        if (darkCnt <= 2) {
//            NSLog(@"SFD");
////            startStateMachine = true;
//            thisFrame = SFD;
//        } else {
//            thisFrame = DATA;
//        }
        if (startStateMachine) {
            short state = 2*lastFrame + 1*thisFrame;
            switch (lastState) {
                case SFDJustBegin:
                    if (thisFrame == SFD) {
                        cntDataFrame = 0;
                        NSLog(@"SFD2;%d", averMin);
                    }
                    else {
                        [self resetState];
                        NSLog(@"error 0:only one SFD frame");
                    }
                    break;
                case SFDWillEnd:
                    if (thisFrame == DATA) {
                        cntDataFrame++;
                        [dataFrameBuffer addObject:[NSValue value:&pixelArr withObjCType:@encode(SamplePixelArray)]];
                    }
                    else {
                        NSLog(@"error 1: more than 2 SFD");
                        [self resetState];
                    }
                    break;
                case DataJustBegin:
                    if (thisFrame == DATA) {
                        cntDataFrame++;
                        [dataFrameBuffer addObject:[NSValue value:&pixelArr withObjCType:@encode(SamplePixelArray)]];
                    }
                    else {
                        NSLog(@"error 2: only one frame of data");
                        [self resetState];
                    }
                    break;
                case DataDidBegin:
                    if (thisFrame == DATA) {
                        cntDataFrame++;
                        [dataFrameBuffer addObject:[NSValue value:&pixelArr withObjCType:@encode(SamplePixelArray)]];
                    }
                    else {
                        if (cntDataFrame%2==0) {
                            if (cntDataFrame > 0) {
                                NSLog(@"Demodulate data:%d", cntDataFrame);
//                                dispatch_async(demodulate_queue, ^{
                                    [self demodulateData];
//                                });
                                cntDataFrame = 0;
                                [dataFrameBuffer removeAllObjects];
                            }
                            else {
                                NSLog(@"reset from error");
                            }
                            NSLog(@"SFD1;%d", averMin);
                        }
                        else {
                            NSLog(@"error 3: data frame numbers is odd, can't demodulate");
                            [self resetState];
                        }
                    }
                    break;
                default:
                    break;
            }
            if (startStateMachine) {
                lastState = state;
                lastFrame = thisFrame;
            }
        }
//        NSString *logStr = [[[NSString alloc] init] autorelease];
//        NSString *sortStr = [[[NSString alloc] init] autorelease];
//        for (int i=0; i<SamplePixelNumbers; i++) {
//            logStr = [logStr stringByAppendingString:[NSString stringWithFormat:@"%d,",pixelArr.array[i]]];
//            sortStr = [sortStr stringByAppendingString:[NSString stringWithFormat:@"%d,",sortArr[i]]];
//        }
//        NSLog(@"%@", sortStr);
//        NSLog(@"%@", logStr);
    }
    
	CVPixelBufferUnlockBaseAddress( pixelBuffer, 0 );
}

-(void) resetState
{
    lastFrame = DATA;
    lastState = DataDidBegin;
    cntDataFrame = 0;
    [dataFrameBuffer removeAllObjects];
    startStateMachine = false;
}

-(void) demodulateData
{
    SamplePixelArray firstArr, secondArr;
    int bitNum = dataFrameBuffer.count/2;
    int data[bitNum];
    for (int i=0; i<bitNum; i++) {
        data[i] = 0;
    }
    for (int i=0; i<dataFrameBuffer.count; i++) {
        if (i%2==0) {
            [[dataFrameBuffer objectAtIndex:i] getValue:&firstArr];
        }
        else {
            [[dataFrameBuffer objectAtIndex:i] getValue:&secondArr];
            int difference[SamplePixelNumbers];
            int cntDiff = 0;
            for (int j=0; j<SamplePixelNumbers; j++) {
                difference[j] = abs(firstArr.array[j] - secondArr.array[j]);
                if (difference[j] < 20) {
                    cntDiff++;
                }
            }
//            NSLog(@"same: %d", cntDiff);
            if (cntDiff < 12) {
//                NSLog(@"%d",1);
                data[(i-1)/2] = 1;
            }
            else {
//                NSLog(@"%d",0);
                data[(i-1)/2] = 0;
            }
        }
    }
    NSString *dataStr = [[[NSString alloc] init] autorelease];
    for (int i=0; i<bitNum; i++) {
        dataStr = [dataStr stringByAppendingString:[NSString stringWithFormat:@"%d",data[i]]];
    }
    NSLog(@"%@", dataStr);
    
}

- (void) demodulateFrame:(unsigned char *)pixel height:(int)height width:(int)width detectColumn:(int)column
{
    
}

#pragma mark Capture

- (void)captureOutput:(AVCaptureOutput *)captureOutput didOutputSampleBuffer:(CMSampleBufferRef)sampleBuffer fromConnection:(AVCaptureConnection *)connection 
{	
	CMFormatDescriptionRef formatDescription = CMSampleBufferGetFormatDescription(sampleBuffer);
    
	if ( connection == videoConnection ) {
		
		// Get framerate
		CMTime timestamp = CMSampleBufferGetPresentationTimeStamp( sampleBuffer );
		[self calculateFramerateAtTimestamp:timestamp];
        
		// Get frame dimensions (for onscreen display)
		if (self.videoDimensions.width == 0 && self.videoDimensions.height == 0)
			self.videoDimensions = CMVideoFormatDescriptionGetDimensions( formatDescription );
		
		// Get buffer type
		if ( self.videoType == 0 )
			self.videoType = CMFormatDescriptionGetMediaSubType( formatDescription );

		CVImageBufferRef pixelBuffer = CMSampleBufferGetImageBuffer(sampleBuffer);
		
		// Synchronously process the pixel buffer to de-green it.
		[self processPixelBuffer:pixelBuffer];
		
		// Enqueue it for preview.  This is a shallow queue, so if image processing is taking too long,
		// we'll drop this frame for preview (this keeps preview latency low).
		OSStatus err = CMBufferQueueEnqueue(previewBufferQueue, sampleBuffer);
		if ( !err ) {        
			dispatch_async(dispatch_get_main_queue(), ^{
				CMSampleBufferRef sbuf = (CMSampleBufferRef)CMBufferQueueDequeueAndRetain(previewBufferQueue);
				if (sbuf) {
					CVImageBufferRef pixBuf = CMSampleBufferGetImageBuffer(sbuf);
					[self.delegate pixelBufferReadyForDisplay:pixBuf];
					CFRelease(sbuf);
				}
			});
		}
	}
    
	CFRetain(sampleBuffer);
	CFRetain(formatDescription);
	dispatch_async(movieWritingQueue, ^{

		if ( assetWriter ) {
		
			BOOL wasReadyToRecord = (readyToRecordAudio && readyToRecordVideo);
			
			if (connection == videoConnection) {
				
				// Initialize the video input if this is not done yet
				if (!readyToRecordVideo)
					readyToRecordVideo = [self setupAssetWriterVideoInput:formatDescription];
				
				// Write video data to file
				if (readyToRecordVideo && readyToRecordAudio)
					[self writeSampleBuffer:sampleBuffer ofType:AVMediaTypeVideo];
			}
			else if (connection == audioConnection) {
				
				// Initialize the audio input if this is not done yet
				if (!readyToRecordAudio)
					readyToRecordAudio = [self setupAssetWriterAudioInput:formatDescription];
				
				// Write audio data to file
				if (readyToRecordAudio && readyToRecordVideo)
					[self writeSampleBuffer:sampleBuffer ofType:AVMediaTypeAudio];
			}
			
			BOOL isReadyToRecord = (readyToRecordAudio && readyToRecordVideo);
			if ( !wasReadyToRecord && isReadyToRecord ) {
				recordingWillBeStarted = NO;
				self.recording = YES;
				[self.delegate recordingDidStart];
			}
		}
		CFRelease(sampleBuffer);
		CFRelease(formatDescription);
	});
}

- (AVCaptureDevice *)videoDeviceWithPosition:(AVCaptureDevicePosition)position 
{
    NSArray *devices = [AVCaptureDevice devicesWithMediaType:AVMediaTypeVideo];
    for (AVCaptureDevice *device in devices)
        if ([device position] == position)
            return device;
    
    return nil;
}

- (AVCaptureDevice *)audioDevice
{
    NSArray *devices = [AVCaptureDevice devicesWithMediaType:AVMediaTypeAudio];
    if ([devices count] > 0)
        return [devices objectAtIndex:0];
    
    return nil;
}

- (void) adjustFocusAndExposurePointOfInterest:(CGPoint)interestedPoint {
    AVCaptureDevice *cameraBack = [self videoDeviceWithPosition:AVCaptureDevicePositionBack];
    if ([cameraBack isAdjustingExposure] || [cameraBack isAdjustingFocus]) {
        NSLog(@"camera is agjusting exposure, return");
        return;
    }
    [cameraBack lockForConfiguration:nil];
    if ([cameraBack isFocusPointOfInterestSupported]) {
        cameraBack.focusPointOfInterest = interestedPoint;
        cameraBack.exposurePointOfInterest = interestedPoint;
        
        [cameraBack setFocusMode:AVCaptureFocusModeAutoFocus];
        [cameraBack setExposureMode:AVCaptureExposureModeContinuousAutoExposure];
    }
    
    [cameraBack unlockForConfiguration];
}

-(void) lockExposure {
    AVCaptureDevice *cameraBack = [self videoDeviceWithPosition:AVCaptureDevicePositionBack];
    [cameraBack lockForConfiguration:nil];
    if ([cameraBack isExposureModeSupported:AVCaptureExposureModeLocked]) {
        [cameraBack setExposureMode:AVCaptureExposureModeLocked];
    }
    [cameraBack unlockForConfiguration];
    
}

- (BOOL) setupCaptureSession 
{
	/*
		Overview: RosyWriter uses separate GCD queues for audio and video capture.  If a single GCD queue
		is used to deliver both audio and video buffers, and our video processing consistently takes
		too long, the delivery queue can back up, resulting in audio being dropped.
		
		When recording, RosyWriter creates a third GCD queue for calls to AVAssetWriter.  This ensures
		that AVAssetWriter is not called to start or finish writing from multiple threads simultaneously.
		
		RosyWriter uses AVCaptureSession's default preset, AVCaptureSessionPresetHigh.
	 */
	 
    /*
	 * Create capture session
	 */
    captureSession = [[AVCaptureSession alloc] init];
    
    /*
	 * Create audio connection
	 */
    AVCaptureDeviceInput *audioIn = [[AVCaptureDeviceInput alloc] initWithDevice:[self audioDevice] error:nil];
    if ([captureSession canAddInput:audioIn])
        [captureSession addInput:audioIn];
	[audioIn release];
	
	AVCaptureAudioDataOutput *audioOut = [[AVCaptureAudioDataOutput alloc] init];
	dispatch_queue_t audioCaptureQueue = dispatch_queue_create("Audio Capture Queue", DISPATCH_QUEUE_SERIAL);
	[audioOut setSampleBufferDelegate:self queue:audioCaptureQueue];
	dispatch_release(audioCaptureQueue);
	if ([captureSession canAddOutput:audioOut])
		[captureSession addOutput:audioOut];
	audioConnection = [audioOut connectionWithMediaType:AVMediaTypeAudio];
	[audioOut release];
    
	/*
	 * Create video connection
	 */
    AVCaptureDevice *cameraBack = [self videoDeviceWithPosition:AVCaptureDevicePositionBack];
//    if ([cameraBack supportsAVCaptureSessionPreset:AVCaptureSessionPreset352x288]) {
//        captureSession.sessionPreset = AVCaptureSessionPreset352x288;
//    }

    if ([cameraBack supportsAVCaptureSessionPreset:AVCaptureSessionPresetiFrame960x540]) {
        captureSession.sessionPreset = AVCaptureSessionPresetiFrame960x540;
    }
//    if ([cameraBack supportsAVCaptureSessionPreset:AVCaptureSessionPreset1920x1080]) {
//        captureSession.sessionPreset = AVCaptureSessionPreset1920x1080;
//    }

//    if ([cameraBack supportsAVCaptureSessionPreset:AVCaptureSessionPreset640x480]) {
//        captureSession.sessionPreset = AVCaptureSessionPreset640x480;
//    }
    AVCaptureDeviceInput *videoIn = [[AVCaptureDeviceInput alloc] initWithDevice:[self videoDeviceWithPosition:AVCaptureDevicePositionBack] error:nil];
    if ([captureSession canAddInput:videoIn])
        [captureSession addInput:videoIn];
	[videoIn release];
    
	AVCaptureVideoDataOutput *videoOut = [[AVCaptureVideoDataOutput alloc] init];
	/*
		RosyWriter prefers to discard late video frames early in the capture pipeline, since its
		processing can take longer than real-time on some platforms (such as iPhone 3GS).
		Clients whose image processing is faster than real-time should consider setting AVCaptureVideoDataOutput's
		alwaysDiscardsLateVideoFrames property to NO. 
	 */
//	[videoOut setAlwaysDiscardsLateVideoFrames:YES];
    [videoOut setAlwaysDiscardsLateVideoFrames:NO];

	[videoOut setVideoSettings:[NSDictionary dictionaryWithObject:[NSNumber numberWithInt:kCVPixelFormatType_32BGRA] forKey:(id)kCVPixelBufferPixelFormatTypeKey]];
	dispatch_queue_t videoCaptureQueue = dispatch_queue_create("Video Capture Queue", DISPATCH_QUEUE_SERIAL);
	[videoOut setSampleBufferDelegate:self queue:videoCaptureQueue];
	dispatch_release(videoCaptureQueue);
	if ([captureSession canAddOutput:videoOut])
		[captureSession addOutput:videoOut];
	videoConnection = [videoOut connectionWithMediaType:AVMediaTypeVideo];
    
    if (videoConnection.supportsVideoMinFrameDuration) {
        videoConnection.videoMinFrameDuration = CMTimeMake(1, 30);
    }
    if (videoConnection.supportsVideoMaxFrameDuration) {
        videoConnection.videoMaxFrameDuration = CMTimeMake(1, 30);
    }

	self.videoOrientation = [videoConnection videoOrientation];
	[videoOut release];
    
	return YES;
}

- (void) setupAndStartCaptureSession
{
	// Create a shallow queue for buffers going to the display for preview.
	OSStatus err = CMBufferQueueCreate(kCFAllocatorDefault, 1, CMBufferQueueGetCallbacksForUnsortedSampleBuffers(), &previewBufferQueue);
	if (err)
		[self showError:[NSError errorWithDomain:NSOSStatusErrorDomain code:err userInfo:nil]];
	
	// Create serial queue for movie writing
	movieWritingQueue = dispatch_queue_create("Movie Writing Queue", DISPATCH_QUEUE_SERIAL);
	
    if ( !captureSession )
		[self setupCaptureSession];
	
    [[NSNotificationCenter defaultCenter] addObserver:self selector:@selector(captureSessionStoppedRunningNotification:) name:AVCaptureSessionDidStopRunningNotification object:captureSession];
	
	if ( !captureSession.isRunning )
		[captureSession startRunning];
}

- (void) pauseCaptureSession
{
	if ( captureSession.isRunning )
		[captureSession stopRunning];
}

- (void) resumeCaptureSession
{
	if ( !captureSession.isRunning )
		[captureSession startRunning];
}

- (void)captureSessionStoppedRunningNotification:(NSNotification *)notification
{
	dispatch_async(movieWritingQueue, ^{
		if ( [self isRecording] ) {
			[self stopRecording];
		}
	});
}

- (void) stopAndTearDownCaptureSession
{
    [captureSession stopRunning];
	if (captureSession)
		[[NSNotificationCenter defaultCenter] removeObserver:self name:AVCaptureSessionDidStopRunningNotification object:captureSession];
	[captureSession release];
	captureSession = nil;
	if (previewBufferQueue) {
		CFRelease(previewBufferQueue);
		previewBufferQueue = NULL;	
	}
	if (movieWritingQueue) {
		dispatch_release(movieWritingQueue);
		movieWritingQueue = NULL;
	}
}

#pragma mark Error Handling

- (void)showError:(NSError *)error
{
    CFRunLoopPerformBlock(CFRunLoopGetMain(), kCFRunLoopCommonModes, ^(void) {
        UIAlertView *alertView = [[UIAlertView alloc] initWithTitle:[error localizedDescription]
                                                            message:[error localizedFailureReason]
                                                           delegate:nil
                                                  cancelButtonTitle:@"OK"
                                                  otherButtonTitles:nil];
        [alertView show];
        [alertView release];
    });
}

@end
