//
//  heuristics.cpp
//  Workflows
//  All there heuristics
//
//  Created by changjiang GOU on 29/11/2017.
//  Copyright © 2017 Changjiang GOU. All rights reserved.
//

#include "heuristics.hpp"

extern bool discrete;

double MaxSpeed(Chain* application){
    return application->getEnergyMaxS();
}

void calculate_SpecialS(Chain* application, double Pbound){
    for (unsigned int i=1; i<=application->getLength(); ++i) {
        application->getNode(i)->setSpecialS(Pbound);
        application->getNode(i)->nonDuplicate();
    }
}

double DuplicateAll(Chain* application,double Pbound, unsigned int num_processor){
    int length=application->getLength();
    if (num_processor>=2*length) {
        for (unsigned int i=1; i<=length; ++i) {
            application->getNode(i)->setSpecialS(Pbound);
            application->getNode(i)->duplicate();
        }
        return application->getEnergy_cost_total();
    }else{
        return -1;//means it fails cause of the shortage of processors
    }
}

bool cmp_weight(Cnode* i, Cnode* j){//for finding the largest (w_i) node
    return i->getWeight()<j->getWeight();
}

//return updated Pexp and Que
void updateQue(Chain* application, std::forward_list<Cnode*> &Que, double &Pexp, bool manybuffers){
    unsigned length=application->getLength();
    application->sortbyMaxEC();//the first element is the largest element, max(w_i/s_i,o_i/beta)
    double bottleneck_time=std::max(application->getNode(1)->getExe_time(),application->getNode(1)->getOut());
    
    Pexp=bottleneck_time;
    Que.clear();
    double execution_time;
    Cnode* curNode;
    if (manybuffers==true) {//many buffers are possible
        for (unsigned int i=1; i<=length; ++i) {
            curNode=application->getNode(i);
            execution_time=curNode->getExe_time();
            if (abs(execution_time-bottleneck_time)<0.000000001) {
                if (!curNode->IsDuplicated()) {
                    Que.push_front(curNode);
                }
                Pexp=Pexp+curNode->getErrorRate()*curNode->getWeight()/SMAX;
            }else{
                break;
            }
        }
    }else{//only two buffers are possible
        double difference;
        for (unsigned int i=1; i<=length; ++i) {
            difference=application->getNode(i)->getvauleLs()-2*bottleneck_time;
            if (difference>0) {
                if (!application->getNode(i)->IsDuplicated()) {
                    Que.push_front(application->getNode(i));
                    Pexp=Pexp+application->getNode(i)->getErrorRate()*difference;
                }
            }
        }
    }
}

double Threshold(Chain* application, double Pbound, double deltaS,unsigned int num_p, std::string &filename){
    //FILE * pFile;
    //pFile=fopen(("./detail/"+filename).c_str(), "w");
    
    bool manybuffers=true;
    calculate_SpecialS(application, Pbound);
    
    int length=application->getLength();
    if (TEST==true) {
        std::cout<<std::endl;
        std::cout<<"Is discrete speed possible: "<<discrete<<", manybuffers possible: "<<manybuffers<<std::endl;
        std::cout<<"Special speed: "<<std::endl;
        for (int i=1; i<=length; ++i) {
            std::cout<<application->getNode(i)->getWeight()<<"|"<<application->getNode(i)->getExe_speed()<<" ";
        }
        std::cout<<std::endl;
        std::cout<<"Running speed: "<<std::endl;
    }
    
    Cnode* smallestNode=application->getSmallPossibleNode(Pbound);
    unsigned int idle_p=num_p-length;
    if (idle_p<=0) {
        return -1;
    }
    
    //double Lc=application->getLargest_commu();
    if (application->getLargest_commu()<Pbound) {
        smallestNode->duplicate();
        --idle_p;
    }
    
    std::forward_list<Cnode*> que;
    double Pexp;
    updateQue(application, que, Pexp, manybuffers);
    
    Cnode* curnode;
    if (Pexp<=Pbound) {
        if (TEST==true) {
            std::cout<<"Pexp is "<<Pexp<<"<=Pbound"<<std::endl;
        }
        
        for (unsigned int i=1; i<=length; i++) {
            curnode=application->getNode(i);
            //std::cout<<curnode->getExe_speed()<<" ";
            if (curnode->getExe_speed()<curnode->getMinESpeed()&&(!curnode->IsDuplicated())) {
                curnode->setMinESpeed();
            }
        }
        
        /*for (int i=1; i<=length; ++i) {
            fprintf(pFile, "%f %d %d",application->getNode(i)->getWeight(),0,application->getNode(i)->IsDuplicated());
            fprintf(pFile,"\n");
        }
        
        fclose(pFile);*/
        return application->getEnergy_cost_total();
    }
    
    double judge, specialS;
    
    struct classcomp {
        bool operator() (const double& lhs, const double& rhs) const
        {return lhs>rhs;}
    };
    std::multimap<double, std::forward_list<Cnode*>::iterator,classcomp> Egain_index;
    
    for (auto iter=que.begin(); iter!=que.end(); ++iter) {
        specialS=(*iter)->getExe_speed();
        
        (*iter)->addSpeed(deltaS);

        //std::cout<<"for weight "<<(*iter)->getWeight()<<", speed is "<<(*iter)->getExe_speed();
        
        if (discrete==true) {
            double diff;
            double speed=specialS;
            for (unsigned int j=0;j<Speed_Options; ++j) {
                diff=speed-DiscreteSpeed[j];
                if (diff<0) {
                    speed=DiscreteSpeed[j];
                    break;
                }
            }
            (*iter)->setExe_speed(speed);
        }
        
        judge=pow((*iter)->getExe_speed(),2)+(*iter)->getErrorRate()*pow(SMAX,2)-2*pow(specialS,2);
        if (judge>0) {
            Egain_index.insert(std::pair<double, std::forward_list<Cnode*>::iterator>((*iter)->getWeight()*judge,iter));
        }
        
        if (idle_p>0) {
            std::multimap<double, std::forward_list<Cnode*>::iterator>::iterator stop;
            if (Egain_index.size()>idle_p) {
                stop=Egain_index.begin();
                std::advance(stop, idle_p);
            }else{
                stop=Egain_index.end();
            }
            
            for (std::multimap<double, std::forward_list<Cnode*>::iterator>::iterator it=Egain_index.begin(); it!=stop; ++it) {
                //std::cout<<"Duplication a task saves energy: "<<it->first<<" ";
                (*(it->second))->duplicate();
                (*(it->second))->setSpecialS(Pbound);
                --idle_p;
            }
            if (TEST==true) {
                std::cout<<std::endl;
            }
        }
        Egain_index.clear();
        
        if (TEST==true) {
            if (!(*iter)->IsDuplicated()) {
                std::cout<<(*iter)->getWeight()<<"|"<<(*iter)->getExe_speed();
                if (manybuffers) {
                    std::cout<<"|"<<(*iter)->getExe_time()-Pbound<<" ";
                }else{
                    std::cout<<"|"<<(*iter)->getvauleLs()-2*Pbound<<" ";
                }
            }
        }
    }
    
    if (TEST==true) {
        std::cout<<std::endl;
        std::cout<<"Duplication time: "<<num_p-length-idle_p<<std::endl;
        updateQue(application, que, Pexp, manybuffers);
        std::cout<<"At last, Que has "<<std::distance(que.begin(), que.end())<<" element, pexp is "<<Pexp<<std::endl;
    }
    
    for (unsigned int i=1; i<=length; i++) {
        curnode=application->getNode(i);
        //std::cout<<curnode->getExe_speed()<<" ";
        if (curnode->getExe_speed()<curnode->getMinESpeed()&&(!curnode->IsDuplicated())) {
            curnode->setMinESpeed();
        }
    }
    
    /*double diff,speed;
    for (int i=1; i<=length; ++i) {
        if (discrete==true) {
            speed=application->getNode(i)->getExe_speed();
            for (unsigned int j=0;j<Speed_Options; ++j) {
                diff=speed-DiscreteSpeed[j];
                if (diff<=0) {
                    break;
                }
            }
        }else{
            diff=(application->getNode(i)->getWeight())/Pbound-application->getNode(i)->getExe_speed();
        }
        fprintf(pFile, "%f %f %d %f",application->getNode(i)->getWeight(),0-diff,application->getNode(i)->IsDuplicated(),application->getNode(i)->getEDerivative());
        fprintf(pFile,"\n");
    }
    
    fclose(pFile);*/
    
    return application->getEnergy_cost_total();
}

double Closer(Chain* application, double Pbound, double DeltaS){
    calculate_SpecialS(application, Pbound);
    
    int length=application->getLength();
    for (unsigned int i=1; i<=length; ++i) {
        application->getNode(i)->setContinuousSpeed(Pbound);
    }
    
    double Pexp;
    std::forward_list<Cnode*> que;
    updateQue(application, que, Pexp, true);
    
    double k=1;
    while (Pexp-Pbound>0.0000001) {
        k=k+DeltaS;
        for (auto iter=que.begin(); iter!=que.end(); ++iter) {
            (*iter)->multiSpeed(k);
        }
        updateQue(application, que, Pexp, true);
    }
    
    Cnode* curnode;
    for (unsigned int i=1; i<=length; i++) {
        curnode=application->getNode(i);
        //std::cout<<curnode->getExe_speed()<<" ";
        if (curnode->getExe_speed()<curnode->getMinESpeed()) {
            curnode->setMinESpeed();
            //std::cout<<"the Ederivative:"<<application->getNode(i)->getEDerivative()<<std::endl;
        }
    }
    
    return application->getEnergy_cost_total();
}

void DuplicationReduceE(Chain* application,double Pt, unsigned int num_p){
    struct classcomp {
        bool operator() (const double& lhs, const double& rhs) const
        {return lhs>rhs;}
    };
    std::multimap<double, int,classcomp> Egain_index;
    long length=application->getLength();
    Cnode* curNode;
    if (num_p>0) {
        double Edup,Enodup;
        for (int i=1; i<=length; ++i) {
            curNode=application->getNode(i);
            Edup=2*pow(curNode->getSpecialS(Pt), 2);
            Enodup=pow(curNode->getExe_speed(), 2)+curNode->getErrorRate()*SMAX*SMAX;
            if (Edup<Enodup) {
                Egain_index.insert(std::pair<double, int>(curNode->getWeight()*(Enodup-Edup),i));
            }
        }
    }
    
    if (num_p>0) {
        std::multimap<double, int>::iterator stop;
        if (Egain_index.size()>num_p) {
            stop=Egain_index.begin();
            std::advance(stop, num_p);
        }else{
            stop=Egain_index.end();
        }
        
        /*if (!Egain_index.empty()) {
            std::cout<<"number of idle processors: "<<num_p<<std::endl;
        }*/
        
        for (std::multimap<double, int>::iterator it=Egain_index.begin(); it!=stop; ++it) {
            //std::cout<<"Duplicating "<<it->second<<" saves energy: "<<it->first<<std::endl;
            application->getNode(it->second)->duplicate();
            application->getNode(it->second)->setSpecialS(Pt);
        }
    }
}

double MiniEnergy(Chain* application,double Pt, int num_p){
    unsigned int length=application->getLength();

    Cnode* curNode;
    for (unsigned int i=1; i<=length; ++i) {
        curNode=application->getNode(i);
        curNode->nonDuplicate();
        curNode->setMinESpeed();
        //std::cout<<curNode->getWeight()<<"|"<<curNode->getMinESpeed()<<" ";
        if (TEST==true) {
            if (abs(curNode->getEDerivative())>0.001) {
                std::cout<<"the Ederivative:"<<application->getNode(i)->getEDerivative()<<", larger than 0, since speed is SMIN "<<curNode->getExe_speed()<<std::endl;
            }
        }
    }
    
    //std::cout<<std::endl;
    
    unsigned int idleNumP=num_p-length;
    application->sortbyWeight();
    double largestweight=application->getNode(1)->getWeight();
    double local_pbound=largestweight/(0.5*SMIN);
    double temps;
    for (unsigned int i=1; i<=length; ++i) {
        temps=application->getNode(i)->getSpecialS(local_pbound);
        if (temps>SMIN) {
            std::cout<<"error!"<<std::endl;
        }
    }
    DuplicationReduceE(application, local_pbound, idleNumP);
    
    return application->getEnergy_cost_total();
}

bool AvailableCheck(Chain* application,double Pbound){
    unsigned int length=application->getLength();
    for (unsigned int i=1; i<=length; ++i) {
        if (application->getNode(i)->getWeight()/SMAX>Pbound) {
            return false;// the application is too computation-intensive
        }
    }
    return true;
}

double BestTrade(Chain* application,double Pt, double probat,unsigned int num_p){
    std::vector<Cnode*> Sreduce,Sfine;
    application->sortbyWeight();//by non-increasing weight
    
    long length=application->getLength();
    unsigned int idlePnumber=num_p-length;
    for (int i=1; i<=length; ++i) {
        application->getNode(i)->setSpecialS(Pt);
        application->getNode(i)->nonDuplicate();
    }
    double ProbaExceed_specialS=application->getProExceeds(Pt);
    
    bool localTEST=false;
    if (localTEST==true) {
        std::cout<<"After setting all speeds as the possible minimal speed, actual probability is "<<ProbaExceed_specialS;
    }
    
    if (ProbaExceed_specialS<=probat) {
        if (localTEST==true) {
            std::cout<<", which is smaller than target. Nothing to do."<<std::endl;
        }
        DuplicationReduceE(application, Pt, idlePnumber);
        return application->getEnergy_cost_total();
    }
    
    if (localTEST==true) {
        std::cout<<", which is larger than target. To increase speed."<<std::endl;
    }
    
    //go to the discrete version
    if (discrete==true) {
        double energy;
        energy=BestTradeDiscrete(application, Pt, probat, num_p);
        return energy;
    }
    
    for (int i=1; i<=length; ++i) {
        application->getNode(i)->SetCriticalS(Pt);
    }
    
    int index=1;
    double NodeWeight=application->getNode(index)->getWeight();
    double upbound=application->getNode(index)->getCriticalS(Pt);
    Sreduce.push_back(application->getNode(index));
    index++;
    Cnode* curNode;
    for (; index<=length; ++index) {
        curNode=application->getNode(index);
        if (curNode->getWeight()==NodeWeight) {
            Sreduce.push_back(curNode);
        }else{
            break;
        }
    }
    
    double sc,sd;
    if (index>length) {
        sc=SMIN;
    }else{sc=application->getNode(index)->getCriticalS(Pt);}
    
    --index;
    sd=application->getNode(index)->getSpecialS(Pt);
    for (int i=1; i<=index; ++i) {
        application->getNode(i)->setExe_speed(std::max(sc,sd));
    }
    ++index;
    
    if (localTEST==true) {
        int j=index-1;
        std::cout<<"\n";
        std::cout<<"Push Node 1 ~ Node "<<j<<" to Sreduce. Critical Speed: "<<sc<<", Special Speed: "<<sd<<std::endl;
    }
    
    double ProbaExceed=application->getProExceeds(Pt);
    
    while (ProbaExceed<probat) {
        if (localTEST==true) {
            std::cout<<"Push Node "<<index;
        }
        NodeWeight=application->getNode(index)->getWeight();
        upbound=application->getNode(index)->getCriticalS(Pt);
        for (; index<=length; ++index) {
            curNode=application->getNode(index);
            if (curNode->getWeight()==NodeWeight) {
                Sreduce.push_back(curNode);
            }else{
                break;
            }
        }
        
        if (index>length) {
            sc=SMIN;
        }else{sc=application->getNode(index)->getCriticalS(Pt);}
        
        sd=application->getNode(index-1)->getSpecialS(Pt);
        
        if (localTEST==true) {
            int j=index-1;
            std::cout<<" ~ Node "<<j<<" to Sreduce. Critical Speed: "<<sc<<", Special Speed: "<<sd<<std::endl;
        }
        
        Sfine.clear();
        for (std::vector<Cnode*>::iterator iter=Sreduce.begin(); iter!=Sreduce.end();) {
            if ((*iter)->getSpecialS(Pt)>std::max(sc,sd)) {
                (*iter)->setSpecialS(Pt);
                
                if (localTEST==true) {
                    std::cout<<"Speical Speed of node "<<(*iter)->getWeight()<<" is "<<(*iter)->getSpecialS(Pt)<<", larger than max(sc,sd), keep running on special speed."<<std::endl;
                }
                Sfine.push_back(*iter);
                Sreduce.erase(iter);
            }else{
                (*iter)->setExe_speed(std::max(sc,sd));
                ++iter;
            }
        }
        
        ProbaExceed=application->getProExceeds(Pt);
    }
    
    for (std::vector<Cnode*>::iterator iter=Sfine.begin(); iter!=Sfine.end();++iter){
        Sreduce.push_back(*iter);
    }
    
    if (localTEST==true) {
        std::cout<<"Actual probability is close to target. ";
    }
    
    double lowbound=std::max(sc,sd),s=lowbound;
    
    double diff=application->getProExceeds(Pt)-probat;

    double percision=pow(10, -16);
    unsigned int multi=10;
    while (diff>0.00001) {
        if (diff>0) {
            s=s+(upbound-s)/2;
        }else{
            s=s-(s-lowbound)/2;
        }
        
        for (std::vector<Cnode*>::iterator iter=Sreduce.begin(); iter!=Sreduce.end();) {
            if (!(*iter)->inSexc(Pt)) {
                Sreduce.erase(iter);
            }else{
                if (s>(*iter)->getSpecialS(Pt)) {
                    (*iter)->setExe_speed(s);
                }
                ++iter;
            }
        }
        
        diff=application->getProExceeds(Pt)-probat;
        
        if (upbound-lowbound<percision) {
            break;
        }else if(lowbound==s){
            s=s+(upbound*multi-lowbound*multi)/(2*multi);
            multi=multi*10;
        }
        
        if (diff>0){
            lowbound=s;
        }else{
            upbound=s;
        }
    }
    
    if (localTEST==true) {
        std::cout<<"Speed of nodes in set Sreduce is "<<s<<std::endl;
    }
    
    DuplicationReduceE(application,Pt, idlePnumber);
    
    return application->getEnergy_cost_total();
}

double BestTradeDiscrete(Chain* application,double Pt, double probat,unsigned int num_p){
    long length=application->getLength();
    
    if (TEST==true) {
        std::cout<<"Set all speeds to their critical speeds."<<std::endl;
    }
    
    for (int i=1; i<=length; ++i) {
        application->getNode(i)->SetCriticalS(Pt);
        application->getNode(i)->nonDuplicate();
    }
    
    std::vector<Cnode*> S_reduce;
    std::vector<double> minimum_larger_Speed;
    double specialS,criticalS,diff;
    Cnode* curNode;
    application->sortbyWeight();//by non-increasing weight
    for (int i=1; i<=length; ++i) {
        curNode=application->getNode(i);
        specialS=curNode->getWeight()/Pt;
        criticalS=curNode->getWeight()/(Pt-curNode->getWeight()/SMAX);
        
        for (unsigned int j=0;j<Speed_Options; ++j) {
            diff=specialS-DiscreteSpeed[j];
            if (diff<=0) {
                if (DiscreteSpeed[j]<criticalS) {
                    minimum_larger_Speed.push_back(DiscreteSpeed[j]);
                    S_reduce.push_back(curNode);
                }
                break;
            }
        }
    }
    
    if (TEST==true) {
        for (std::vector<Cnode*>::iterator iter=S_reduce.begin(); iter!=S_reduce.end(); ++iter) {
            std::cout<<(*iter)->getWeight()<<"--"<<minimum_larger_Speed[iter-S_reduce.begin()]<<"  ";
        }
    }
    
    double ProbaExceed=application->getProExceeds(Pt);
    unsigned int k=0;
    unsigned long vector_length=S_reduce.size();
    while (ProbaExceed<probat&k<vector_length) {
        S_reduce[k]->setExe_speed(minimum_larger_Speed[k]);
        ProbaExceed=application->getProExceeds(Pt);
        k=k+1;
    }
    if (ProbaExceed>probat) {
        k=k-1;
        S_reduce[k]->SetCriticalS(Pt);
    }
    
    DuplicationReduceE(application, Pt, num_p);
    
    double energy=application->getEnergy_cost_total();
    return energy;
}

void calPexpPro(Chain* application, double Pbound, double & expectedP, double & actualPro){
    double Pnf=application->getPnf();
    expectedP=Pnf;
    long length=application->getLength();
    Cnode* curNode;
    for (int i=1; i<=length; ++i) {
        curNode=application->getNode(i);
        if(curNode->getExe_time()-Pnf>0.000000001){
            expectedP=expectedP+curNode->getErrorRate()*curNode->getWeight()/SMAX;
        }
    }
    
    actualPro=application->getProExceeds(Pbound);
}
