# Source lesson S: finite-net boundedness (a93A01 warm-up component)
This is TA teaching from the familiar a93A01 development, not fresh transfer evidence.
A map f:(0,1)→R is uniformly continuous when for every ε>0 there is δ>0 such that |x-y|<δ implies |f(x)-f(y)|<ε, uniformly in x,y.
Choose the tolerance δ for ε=1. Choose integer m with 1/(2m)<δ. The m midpoint centers c_i=(i+1/2)/m, 0≤i<m, lie inside (0,1), and every point lies within δ of one center. Thus |f(x)|<1+max_i|f(c_i)|. Finiteness makes the maximum meaningful; compactness of the open interval is not asserted.
General condition: total boundedness means a finite radius-r net for every r>0, with centers in the space. A bounded metric space need not have this property. The canonical bounded-image pattern's phrase “bounded — or totally bounded” overstates its valid generality. Treat that text as an authored method requiring premise review.
Source: TA prototype b9e09bb5, a93A01 cascade B/B1/B2/B3, and its pinned preliminary development. This lesson is TA-authored; it is not a reviewed memory.
