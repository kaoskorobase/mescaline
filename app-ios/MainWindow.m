#import <UIKit/UIKit.h>

extern int Haskell_main(int argc, char* argv[]);

int main(int argc, char* argv[])
{
	Haskell_main(argc, argv);
}

int doMain(int argc, char *argv[]) {
    
    NSAutoreleasePool * pool = [[NSAutoreleasePool alloc] init];
    int retVal = UIApplicationMain(argc, argv, nil, nil);
    [pool release];
    return retVal;
}

int openWindow(void);

int openWindow() {
    static char* args[2];
    args[0] = "dummy";
    args[1] = NULL;
    return doMain(1, args);
}
