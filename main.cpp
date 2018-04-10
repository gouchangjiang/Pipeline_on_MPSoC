//
//  main.cpp
//  Workflows
//
//  Created by changjiang GOU on 29/11/2017.
//  Copyright © 2017 Changjiang GOU. All rights reserved.
//
#include <fstream>
#include "heuristics.hpp"
#include "utils.hpp"

extern bool discrete;
int main(int argc, const char * argv[]) {
    int application_size=0;
    double *ewghts,*nwghts;
    std::string dir=argv[1];
    std::string filename;
    const unsigned int NUM_P=512;
    double Pbound=0,k;
    double deltaS = 15;//1 percent
    double deltaS_multi = 0.001;// for multiplying the speed
    double probability,expectedP,actualPro,npratio;
    if (!strcmp(argv[5],"true")) {
        discrete=true;
    }
    bool manybuffers=false;
    if (!strcmp(argv[6],"true")) {
        manybuffers=true;
    }
    double energy_MaxS,energy_DupAll,energy_Threshold,energy_Mini,energy_closer,energy_bestT;
    
    std::cout.precision(10);
    std::cout<<"Tree_name "<<"NPRatio "<<"Heuristics "<<"Energy "<<"ExpectedP "<<"ActualProba "<<"K "<<"Pbound "<<"ProbaT"<<std::endl;
    
    std::ifstream OpenFile(dir+argv[2]);
    std::ifstream PboundFile(dir+argv[3]);
    std::ifstream PpFile(dir+argv[4]);
    std::string buffer;
    char cur_char;
    std::vector<double> targetPeriod;
    std::vector<double> ProbabilityT;
    do{
        OpenFile>>filename;
        parse_tree((dir+filename).c_str(), &application_size, &nwghts, &ewghts);
        
        do{
            PboundFile>>buffer;
            targetPeriod.push_back(std::stod(buffer));
            cur_char = PboundFile.get();
        }while(cur_char != '\n' && PboundFile.good());
        
        do{
            PpFile>>buffer;
            ProbabilityT.push_back(std::stod(buffer));
            cur_char = PpFile.get();
        }while(cur_char != '\n' && PpFile.good());
        
        //PboundFile>>Pbound>>probability;
        
        Chain *app = new Chain(application_size,nwghts,ewghts);
        npratio=double(application_size)/NUM_P;
        
        //k=0.95;//k is a coeficient defiend by the data
        k=0.4;
        while (!targetPeriod.empty()) {
            Pbound=targetPeriod.back();
            targetPeriod.pop_back();
            probability=ProbabilityT.back();
            ProbabilityT.pop_back();
            
            bool avaiable=AvailableCheck(app, Pbound);
            
            if (avaiable==true) {
                energy_MaxS=MaxSpeed(app);
                calPexpPro(app, Pbound, expectedP, actualPro);
                std::cout<<filename<<" "<<npratio<<" MaxS "<<energy_MaxS<<" "<<expectedP<<" "<<actualPro<<" "<<k<<" "<<Pbound<<" "<<probability<<std::endl;
                
                energy_DupAll=DuplicateAll(app, Pbound, NUM_P);
                calPexpPro(app, Pbound, expectedP, actualPro);
                std::cout<<filename<<" "<<npratio<<" DupAll "<<energy_DupAll<<" "<<expectedP<<" "<<actualPro<<" "<<k<<" "<<Pbound<<" "<<probability<<std::endl;
                
                energy_Mini=MiniEnergy(app,Pbound,NUM_P);
                calPexpPro(app, Pbound, expectedP, actualPro);
                std::cout<<filename<<" "<<npratio<<" BestE "<<energy_Mini<<" "<<expectedP<<" "<<actualPro<<" "<<k<<" "<<Pbound<<" "<<probability<<std::endl;
                
                energy_Threshold=Threshold(app,Pbound,deltaS, NUM_P,filename);
                calPexpPro(app, Pbound, expectedP, actualPro);
                std::cout<<filename<<" "<<npratio<<" Threshold "<<energy_Threshold<<" "<<expectedP<<" "<<actualPro<<" "<<k<<" "<<Pbound<<" "<<probability<<std::endl;
                
                energy_closer=Closer(app, Pbound, deltaS_multi);
                calPexpPro(app, Pbound, expectedP, actualPro);
                std::cout<<filename<<" "<<npratio<<" Closer "<<energy_closer<<" "<<expectedP<<" "<<actualPro<<" "<<k<<" "<<Pbound<<" "<<probability<<std::endl;
                
                energy_bestT=BestTrade(app ,Pbound, probability,NUM_P);
                calPexpPro(app, Pbound, expectedP, actualPro);
                std::cout<<filename<<" "<<npratio<<" BestTrade "<<energy_bestT<<" "<<expectedP<<" "<<actualPro<<" "<<k<<" "<<Pbound<<" "<<probability<<std::endl;
            }else{
                std::cout<<filename<<" "<<npratio<<" Failure "<<"na"<<" "<<"na"<<" "<<"na"<<" "<<k<<" "<<Pbound<<" "<<probability<<std::endl;
            }
            //k=k-0.01;
        }
        
        delete app;
        delete[] ewghts;
        delete[] nwghts;
    }while (OpenFile.good());
    OpenFile.close();
    PboundFile.close();
    
    return 0;
}
