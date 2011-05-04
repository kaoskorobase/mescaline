/*
 *  GlobalTypes.h
 *  Mescaline
 *
 *  Created by maule on 04.05.11.
 *  Copyright 2011 __MyCompanyName__. All rights reserved.
 *
 */
#ifndef GLOBALTYPES_INCLUDED
#define GLOBALTYPES_INCLUDED

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

#endif // GLOBALTYPES_INCLUDED
