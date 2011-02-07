#import <UIKit/UIKit.h>

@interface MescalineViewController : UIViewController {
	CGPoint lastPoint;
	UIImageView *drawImage;
	BOOL mouseSwiped;	
	int mouseMoved;
        void (*cb_touchesBegan)(void* vc, UITouch* touch);
        void (*cb_touchesMoved)(void* vc, UITouch* touch);
        void (*cb_touchesEnded)(void* vc, UITouch* touch);
}

@end

