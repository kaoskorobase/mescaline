//
//  View.m
//  Mescaline
//
//  Created by Stefan Kersten on 30.03.11.
//  Copyright 2011 __MyCompanyName__. All rights reserved.
//

#import "View.h"

#include <cairo/cairo.h>
#include <cairo/cairo-quartz.h>

#ifndef CAIRO_HAS_QUARTZ_SURFACE
#  error Need to build Cairo with Quartz support (version 1.4.0 or higher)
#endif

@implementation View

//- (BOOL)isOpaque
//{
//	return NO;
//}

- (id)initWithFrame:(CGRect)frame
{
    self = [super initWithFrame:frame];
    if (self) {
        m_cairoSurface = 0;
		m_cairoContext = 0;
    }
    return self;
}

- (void)initCairo:(CGContextRef)ctx
{
	[self destroyCairo];

	CGRect bounds = [self bounds];
	int width = bounds.size.width;
	int height = bounds.size.height;

	// Make the CGContext coordinate system sane, as expected by Cairo
	CGContextTranslateCTM (ctx, 0.0, height);
	CGContextScaleCTM (ctx, 1.0, -1.0);
	
	// Create the Cairo surface and context
	m_cairoSurface = cairo_quartz_surface_create_for_cg_context(ctx, width, height);
	m_cairoContext = cairo_create(m_cairoSurface);	
}

- (void)destroyCairo
{
	if (m_cairoContext) cairo_destroy(m_cairoContext);
	if (m_cairoSurface) cairo_surface_destroy(m_cairoSurface);
}

- (void)dealloc
{
	[self destroyCairo];
    [super dealloc];
}

- (cairo_t*)getCairoContext
{
	CGContextRef ctx = UIGraphicsGetCurrentContext();
	if ((m_cairoSurface == NULL) || (ctx !=  cairo_quartz_surface_get_cg_context(m_cairoSurface))) {
		[self initCairo:ctx];
	}
	return m_cairoContext;
}

- (void)drawRect:(CGRect)rect
{
	[self redraw: [self getCairoContext] inRect:rect];

}

static float points [][2] = {
	{0.9649 , 0.6787},
	{0.1576 , 0.7577},
	{0.9706 , 0.7431},
	{0.9572 , 0.3922},
	{0.4854 , 0.6555},
	{0.8003 , 0.1712},
	{0.1419 , 0.7060},
	{0.4218 , 0.0318},
	{0.9157 , 0.2769},
	{0.7922 , 0.0462},
	{0.9595 , 0.0971},
	{0.6557 , 0.8235},
	{0.0357 , 0.6948},
	{0.8491 , 0.3171},
	{0.9340 , 0.9502}};

static float regions [][2] = {
	{0.6843 , 0.5320},
	{0.2295 , 0.8974}};

static float colors [][3] = {
	{ 1, 0, 0},
	{ 0, 1, 0},
	{ 0, 0, 1},
	{ 0, 1, 1},
	{ 1, 0, 1},
	{ 1, 1, 0}};

- (void)redraw:(cairo_t*)cr inRect:(CGRect)rect
{
	CGRect bounds = [self bounds];
	int width = bounds.size.width;
	int height = bounds.size.height;

	// Cairo-Quartz fallback surfaces don't work properly, so we need to   create a temp. surface like this:
	cairo_push_group(cr);
	
	//---------- Drawing stuff (put your code in here)   -------------------------------
	// Draw a radial gradient (copied and pasted, more or less, from   http://cairographics.org/samples/gradient.html)
	
	for (int i = 0; i < 15; i++){
		cairo_arc(cr, points[i][0]*width, points[i][1]*height, 5, 0, 2 * M_PI);
		cairo_set_source_rgba(cr, 0, 0, 0, 1);
		cairo_stroke(cr);
	}
	
	for (int i = 0; i < 2; i++){
		cairo_arc(cr, regions[i][0]*height, regions[i][1]*width, 30, 0, 2 * M_PI);
		cairo_set_source_rgba(cr, colors[i][0], colors[i][1], colors[i][2], .5);
		cairo_fill_preserve(cr);
		cairo_set_source_rgba(cr, 0, 0, 0, 1);
		cairo_stroke(cr);
	}
	
	//--------------------------------------------------------------------------------
	
	// Finally, paint the temporary surface we made
	cairo_pop_group_to_source(cr);
	cairo_paint(cr);	
}
@end 

//@implementation View
//
//
//- (id)initWithFrame:(CGRect)frame {
//    
//    self = [super initWithFrame:frame];
//    if (self) {
//        // Initialization code.
//    }
//    return self;
//}
//
///*
//// Only override drawRect: if you perform custom drawing.
//// An empty implementation adversely affects performance during animation.
//- (void)drawRect:(CGRect)rect {
//    // Drawing code.
//}
//*/
//
//- (void)dealloc {
//    [super dealloc];
//}
//
//
//@end

//@implementation View
////START:code.drawing.path
//- (CGMutablePathRef) triangle {
//    CGMutablePathRef path = CGPathCreateMutable();
//    CGPathMoveToPoint(path, NULL, 0,-173);
//    CGPathAddLineToPoint(path, NULL, 200,173);
//    CGPathAddLineToPoint(path, NULL,-200,173);
//    CGPathCloseSubpath(path);
//    return path;
//}
////END:code.drawing.path
////START:code.drawing.fill
//- (void) fill: (CGMutablePathRef) path 
//    withColor:(UIColor *) color
//    inContext: (CGContextRef) ctx  {
//    CGContextSetFillColorWithColor(ctx, color.CGColor);
//    CGContextAddPath(ctx, path);
//    CGContextFillPath(ctx);    
//}
//- (void) stroke:(CGMutablePathRef) path
//      withColor:(UIColor *) color
//          width:(CGFloat) width
//      inContext:(CGContextRef) ctx {
//    CGContextSetStrokeColorWithColor(ctx, color.CGColor);
//    CGContextSetLineWidth(ctx, width);
//    CGContextAddPath(ctx, path);
//    CGContextStrokePath(ctx);
//}
////END:code.drawing.fill
////START:code.drawing.center
//-(void) centerContext:(CGContextRef) ctx {
//    CGPoint center = [self convertPoint:self.center fromView:nil];
//    CGContextSaveGState(ctx);
//    CGContextTranslateCTM(ctx, center.x, center.y);       
//}
//-(void) restoreContext:(CGContextRef) ctx {
//    CGContextRestoreGState(ctx);
//}
////END:code.drawing.center
////START:code.drawing.draw
//- (void)drawRect:(CGRect)rect {
//    CGMutablePathRef triangle  = [self triangle];
//    CGContextRef ctx = UIGraphicsGetCurrentContext();
//    [self centerContext:ctx];
//    [self fill:triangle 
//     withColor:[UIColor yellowColor] 
//     inContext:ctx];
//    [self stroke:triangle 
//       withColor:[UIColor blackColor] 
//           width:20.0 
//       inContext:ctx];
//    [self restoreContext:ctx];
//    CGPathRelease(triangle);
//}
////END:code.drawing.draw
//- (void)dealloc {
//    [super dealloc];
//}
//
//
//@end
