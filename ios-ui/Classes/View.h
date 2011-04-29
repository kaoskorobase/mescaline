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
#include <vector>
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

//@property (nonatomic, retain) NSMutableArray  *hixihaxi;

- (void)initCairo:(CGContextRef)ctx;
- (void)destroyCairo;
- (cairo_t*)getCairoContext;
- (void)redraw:(cairo_t*)cr inRect:(CGRect)rect;



@end
