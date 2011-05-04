//
//  View.h
//  Mescaline
//
//  Created by Stefan Kersten on 30.03.11.
//  Copyright 2011 __MyCompanyName__. All rights reserved.
//

#import <UIKit/UIKit.h>
#include <cairo/cairo.h>
#import "FakeModel.h"
#include "GlobalTypes.h"

@interface View : UIView
{
	cairo_surface_t* m_cairoSurface;
	cairo_t* m_cairoContext;
	FakeModel *fmodel;
@private
//	RegionList rs;
//	PointList ps;
	BOOL drag;

}

//@property PointList ps;

- (void)initCairo:(CGContextRef)ctx;
- (void)destroyCairo;
- (cairo_t*)getCairoContext;
- (void)redraw:(cairo_t*)cr inRect:(CGRect)rect;


@end
