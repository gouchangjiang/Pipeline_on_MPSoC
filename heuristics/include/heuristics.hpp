//
//  heuristics.hpp
//  Workflows
//
//  Created by changjiang GOU on 29/11/2017.
//  Copyright © 2017 Changjiang GOU. All rights reserved.
//

#ifndef heuristics_hpp
#define heuristics_hpp

#include "chains.hpp"
#include <forward_list>
#include <map>

double MaxSpeed(Chain* application);
double DuplicateAll(Chain* application,double Pbound, unsigned int num_processor);
double Threshold(Chain* application, double Pbound, double deltaS,unsigned int num_p,std::string &filename);
double Closer(Chain* application, double Pbound, double DeltaS);
double MiniEnergy(Chain* application,double Pt, int num_p);
bool AvailableCheck(Chain* application,double Pbound);
double BestTrade(Chain* application,double Pt, double probat,unsigned int num_p);
double BestTradeDiscrete(Chain* application,double Pt, double probat,unsigned int num_p);
void calPexpPro(Chain* application, double Pbound, double & expectedP, double & actualPro);

#endif /* heuristics_hpp */

