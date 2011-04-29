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

- (FakeModel *)fmodel
{
	if (!fmodel) {
		fmodel = [[FakeModel alloc] init];
	}
	return fmodel;
}

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




int numPoints = 15;
int active = rand()%numPoints;

int numRegions = 6;

typedef std::vector<int> int_vec_t;
std::vector<int> myRegions(numRegions); 


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
	
	RegionList rs;
	for (int i=0; i < r; i++) {
		float x = rand()/(float)RAND_MAX;
		float y = rand()/(float)RAND_MAX;
		float rad = rand()/(float)RAND_MAX;
		rs.push_back(Mescaline::Region(x,y,rad+100));
		//rs.push_back(Mescaline::Region(0.3,0.23,100));
		myRegions[i] = 0;

	}

	return rs;
}

static PointList ps = makePoints(numPoints);
static RegionList rs = makeRegions(numRegions);


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
		//int r = arc4random() % 74;
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

	float alpha_featureSpace = 0.7;
	float alpha_sequencer = 1;
	
	// Cairo-Quartz fallback surfaces don't work properly, so we need to   create a temp. surface like this:
	cairo_push_group(cr);
//	int numPoints = 15;
//	int active = rand()%numPoints;
//	
//	int numRegions = 1;
//	ps = makePoints(numPoints);
//	rs = makeRegions(numRegions);
	drawFeatureSpace(ps, active, alpha_featureSpace, cr, bounds);
	drawRegions(rs,alpha_featureSpace-.2, cr, bounds);
	//drawSequencer(0.8, alpha_sequencer, cr, bounds);
	cairo_pop_group_to_source(cr);
	cairo_paint(cr);	

}
- (BOOL)checkIfOverRegion:(CGPoint)currentPosition
{
	
	CGRect bounds = [self bounds];
	int width = bounds.size.width;
	int height = bounds.size.height;
	BOOL ret;
	for (int i = 0; i < rs.size(); i++){
		double dist = sqrt(pow((rs[i].x()*height - currentPosition.x),2)  + pow((rs[i].y()*width - currentPosition.y),2));
		if (dist<=rs[i].size()) {
			NSLog(@"%s\t%f\t%f","over circle",rs[i].x()*height,currentPosition.x);
			ret = YES;
			myRegions.at(i) = 1;
			break;
		} else {
			NSLog(@"%s\t%d\t%f","NOT over circle number:",i,dist);
			myRegions.at(i) = 0;
			ret = NO;
		}
		NSLog(@"%s\t%f\t%f\t%f\t%f","cx, mx, cy, my",rs[i].x()*height, currentPosition.x, rs[i].y()*width, currentPosition.y);	
		NSLog(@"%s\t%f\t%f\t%f\t%f","cx, mx, cy, my",rs[i].x()*height, currentPosition.x, rs[i].y()*width, currentPosition.y);	
	}
	return ret;
}

- (void)touchesBegan:(NSSet *)touches withEvent:(UIEvent *)event{
	NSUInteger numTaps = [[touches anyObject] tapCount];
	NSUInteger numTouches = [touches count];
	
	UITouch *touch = [touches anyObject];
	CGPoint startPoint = [touch locationInView:self];
//	[self checkIfOverRegion:(startPoint)];
	if ([self checkIfOverRegion:(startPoint)]){
		NSLog(@"i start drawing");

		drag = YES;
	} else {
		drag = NO;
	}

	//	NSLog(@"%f", startPoint.x);
	//	NSLog(@"%f", startPoint.y);
}
- (void)touchesEnded:(NSSet *)touches withEvent:(UIEvent *)event{
	for(int i=0;i<rs.size();i++){
		myRegions.at(i) = 0;
	}
}
- (void)touchesMoved:(NSSet *)touches withEvent:(UIEvent *)event{
	UITouch *touch = [touches anyObject];
	CGPoint currentPosition = [touch locationInView:self];
	if (drag) {
		// get the position from the model
		for(int i=0;i<rs.size();i++){
			if(myRegions.at(i) == 1){
				CGPoint myPos = [[self fmodel] setPosition:currentPosition];
				rs[i] = Mescaline::Region(myPos.x/self.frame.size.height,myPos.y/self.frame.size.width,100);
			}
		}
		[self setNeedsDisplay];

	}
	
	//NSLog(@"%f", myPos.x);
	//	NSLog(@"%f", currentPosition.y);
	
	
}
@end 

