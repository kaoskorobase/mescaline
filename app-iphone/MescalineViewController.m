#import "MescalineViewController.h"

@implementation MescalineViewController


static void (*cb_viewDidLoad)(void*);

void setViewDidLoad(void (*fp)(void*))
{
    cb_viewDidLoad = fp;
}

void setTouchesBegan(MescalineViewController* vc, void (*cb)(void* vc, UITouch* touch))
{
    vc->cb_touchesBegan = cb;
}

void setTouchesMoved(MescalineViewController* vc, void (*cb)(void* vc, UITouch* touch))
{
    vc->cb_touchesMoved = cb;
}

void setTouchesEnded(MescalineViewController* vc, void (*cb)(void* vc, UITouch* touch))
{
    vc->cb_touchesEnded = cb;
}

double getTouchX(MescalineViewController* vc, UITouch* touch)
{
    CGPoint currentPoint = [touch locationInView:vc.view];
    return currentPoint.x;
}

double getTouchY(MescalineViewController* vc, UITouch* touch)
{
    CGPoint currentPoint = [touch locationInView:vc.view];
    return currentPoint.y;
}

int getTouchTapCount(UITouch* touch)
{
    return [touch tapCount];
}

- (void)viewDidLoad {
    [super viewDidLoad];
    cb_touchesBegan = NULL;
    cb_touchesMoved = NULL;
    cb_touchesEnded = NULL;
    cb_viewDidLoad(self);
    drawImage = [[UIImageView alloc] initWithImage:nil];
    drawImage.frame = self.view.frame;
    [self.view addSubview:drawImage];
    self.view.backgroundColor = [UIColor lightGrayColor];
    mouseMoved = 0;
}

- (void)touchesBegan:(NSSet *)touches withEvent:(UIEvent *)event {
    UITouch *touch = [touches anyObject];
    if (cb_touchesBegan != NULL)
        cb_touchesBegan(self, touch);
}

- (void)_erase {
    drawImage.image = nil;
}

void erase(MescalineViewController* vc)
{
    [vc _erase];
}

- (void)_beginImageContext {
    UIGraphicsBeginImageContext(self.view.frame.size);
    [drawImage.image drawInRect:CGRectMake(0, 0, self.view.frame.size.width, self.view.frame.size.height)];
    CGContextSetLineCap(UIGraphicsGetCurrentContext(), kCGLineCapRound);
    CGContextSetLineWidth(UIGraphicsGetCurrentContext(), 4.0);
    CGContextSetRGBStrokeColor(UIGraphicsGetCurrentContext(), 1.0, 0.2, 0.0, 1.0);
}

void beginImageContext(MescalineViewController* vc)
{
    [vc _beginImageContext];
}

- (void)_endImageContext {
    drawImage.image = UIGraphicsGetImageFromCurrentImageContext();
    UIGraphicsEndImageContext();
}

void endImageContext(MescalineViewController* vc)
{
    [vc _endImageContext];
}

void drawLine(double x0, double y0, double x1, double y1)
{
    CGContextBeginPath(UIGraphicsGetCurrentContext());
    CGContextMoveToPoint(UIGraphicsGetCurrentContext(), x0, y0);
    CGContextAddLineToPoint(UIGraphicsGetCurrentContext(), x1, y1);
    CGContextStrokePath(UIGraphicsGetCurrentContext());
}

- (void)touchesMoved:(NSSet *)touches withEvent:(UIEvent *)event {
    UITouch *touch = [touches anyObject];	
    if (cb_touchesMoved != NULL)
        cb_touchesMoved(self, touch);
}

- (void)touchesEnded:(NSSet *)touches withEvent:(UIEvent *)event {
    UITouch *touch = [touches anyObject];	
    if (cb_touchesEnded != NULL)
        cb_touchesEnded(self, touch);
}


- (void)didReceiveMemoryWarning {
    [super didReceiveMemoryWarning];
}


- (void)dealloc {
    [super dealloc];
}

@end
