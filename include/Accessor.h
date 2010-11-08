#ifndef ACCESSOR_H_INCLUDED
#define ACCESSOR_H_INCLUDED

#define ACCESSOR(N,F,T,V) \
N :: Accessor (T) (V) ; \
N = accessor F (\x a -> a { F = x })

#endif /* ACCESSOR_H_INCLUDED */
