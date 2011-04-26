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
#include <vector>

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


static float colors [][3] = {
	{ 1, 0, 0},
	{ 0, 1, 0},
	{ 0, 0, 1},
	{ 0, 1, 1},
	{ 1, 0, 1},
	{ 1, 1, 0}};


namespace Mescaline
{
class Point
{
public:
	Point(float x, float y)
		: m_x(x), m_y(y)
	{ }

	float x() const { return m_x; }
	float y() const { return m_y; }

private:
	float m_x;
	float m_y;
};
	
class Region
{
public:
	Region(float x, float y, float size)
	: m_x(x), m_y(y), m_size(size)
	{ }
	
	float x() const { return m_x; }
	float y() const { return m_y; }
	float size() const { return m_size; }
	
private:
	float m_x;
	float m_y;
	float m_size;
};
	
};

typedef std::vector<Mescaline::Point> PointList;
typedef std::vector<Mescaline::Region> RegionList;


PointList makePoints(int r)
{	
	
	PointList ps;
	for (int i=0; i < r; i++) {

		float x = rand()/(float)RAND_MAX;
		float y = rand()/(float)RAND_MAX;
		ps.push_back(Mescaline::Point(x, y));
	}
	
	return ps;
}


RegionList makeRegions(int r)
{	
	
	RegionList ps;
	for (int i=0; i < r; i++) {
		float x = rand()/(float)RAND_MAX;
		float y = rand()/(float)RAND_MAX;
		float rad = rand()/(float)RAND_MAX;
		ps.push_back(Mescaline::Region(x,y,rad+100));
	}

	return ps;
}

void drawFeatureSpace(const PointList& points_array, int active, float alpha, cairo_t* cr, CGRect bounds)
{
	
	int width = bounds.size.width;
	int height = bounds.size.height;

	int size = 5;
	
	for (int i = 0; i < points_array.size(); i++){
		cairo_arc(cr, points_array[i].x()*width, points_array[i].y()*height, size, 0, 2 * M_PI);
		cairo_set_source_rgba(cr, 0, 0, 0, alpha);
		
		if (i != active) cairo_fill_preserve(cr);
		
		cairo_stroke(cr);
	}
}

void drawRegions(const RegionList& regions_array, float alpha, cairo_t* cr, CGRect bounds)
{
	
	int width = bounds.size.width;
	int height = bounds.size.height;
	
	for (int i = 0; i < regions_array.size(); i++){
		cairo_arc(cr, regions_array[i].x()*height, regions_array[i].y()*width, regions_array[i].size(), 0, 2 * M_PI);
		cairo_set_source_rgba(cr, colors[i][0], colors[i][1], colors[i][2], alpha);
		cairo_fill_preserve(cr);
		//cairo_set_source_rgba(cr, 0, 0, 0, alpha);
		cairo_stroke(cr);
	}
}


void drawSequencer(float size, float alpha, cairo_t* cr, CGRect bounds)
{
	
	int width = bounds.size.width;
	int height = bounds.size.height;

	int numRows = 8;
	
	int ref; 
	if (width < height) ref = width;
	else ref = height;
	
	int a = ref*size;
	
	int offset = a % numRows;
	a = a - offset;
	
	int x = width/2 - a/2;
	int y = height/2 - a/2;
	
	//the outer rectangle
	//cairo_rectangle(cr, x, y, a, a);
	//cairo_set_source_rgba(cr, 0, 0, 0, alpha);
	//cairo_stroke(cr);
	
	
	int y_start = y;
	int b = a/numRows;
	
	for (int i = 0; i<numRows; i++){
		int x_start = x;
		for (int j = 0; j < numRows; j++){
		
			cairo_rectangle(cr, x_start, y_start, b, b);
			cairo_set_source_rgba(cr, 0, 0, 0, alpha);
			cairo_stroke(cr);
			x_start += b;
		}
		y_start += b;
	}
	
	
}


- (void)redraw:(cairo_t*)cr inRect:(CGRect)rect
{
	srand(time(0));
	
	CGRect bounds = [self bounds];

	float alpha_featureSpace = 0.3;
	float alpha_sequencer = 1;
	
	// Cairo-Quartz fallback surfaces don't work properly, so we need to   create a temp. surface like this:
	cairo_push_group(cr);
	
	//---------- Drawing stuff (put your code in here)   -------------------------------
	// Draw a radial gradient (copied and pasted, more or less, from   http://cairographics.org/samples/gradient.html)
	
	int numPoints = 15;
	int active = rand()%numPoints;
	
	int numRegions = 2;
	
	drawFeatureSpace(makePoints(numPoints), active, alpha_featureSpace, cr, bounds);
	drawRegions(makeRegions(numRegions),alpha_featureSpace-.2, cr, bounds);
	drawSequencer(0.8, alpha_sequencer, cr, bounds);
	
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
