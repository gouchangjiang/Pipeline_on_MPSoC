//
//  chains.hpp
//  Workflows
//
//  Created by changjiang GOU on 29/11/2017.
//  Copyright © 2017 Changjiang GOU. All rights reserved.
//

#ifndef chains_hpp
#define chains_hpp

#include <iostream>
#include <vector>
#include <array>
#include <math.h>
#include <algorithm>

//all configuration for processor, failure rate
const double Lambda_zero=pow(10, -6);
const double TildeD=4;
const double SMAX=1228.8;
const double SMIN=66;
const double BETA=1;//communication bandwidth;
const unsigned Speed_Options=6;
const std::array<const double, Speed_Options> DiscreteSpeed={66,260,495,730,965,1228.8};
const bool TEST=0;
const double S_R=1023.100596;

extern bool discrete;

class Cnode {
private:
    double w,in,out,exe_speed,exe_time,minESpeed,speed_continuous;
    bool duplicated,minEScal=false;
public:
    Cnode(){
        w=0;
        in=0;
        out=0;
        exe_time=0;
        duplicated=false;
        minEScal=false;
    }
    
    Cnode(double weight, double input, double output){
        w=weight;
        in=input;
        out=output;
        exe_time=0;
        duplicated=false;
        minEScal=false;
    }
    
    void setWeight(double weight){
        w=weight;
    }
    double const getWeight()const {
        return w;
    }
    
    void setInput(double input){
        in=input;
    }
    double const getIn() const {
        return in;
    }
    
    void setOutput(double output){
        out=output;
    }
    double const getOut() const {
        return out;
    }
    
    double getExe_time() {
        exe_time=w/exe_speed;
        return exe_time;
    }
    
    double getErrorRate() const {
        double error_rate;
        if (duplicated) {
            return 0;
        }else{
            error_rate=Lambda_zero*exp(TildeD*(SMAX-exe_speed)/(SMAX-SMIN))*(w/exe_speed);//All these constants defined in the corresponding header file
        }
        return error_rate;
    }
    
    double getErrorRate(double speed) const {
        double error_rate;
        if (duplicated) {
            return 0;
        }else{
            error_rate=Lambda_zero*exp(TildeD*(SMAX-speed)/(SMAX-SMIN))*(w/speed);//All these constants defined in the corresponding header file
        }
        return error_rate;
    }
    
    void duplicate(){
        duplicated=true;
    }
    
    void nonDuplicate(){
        duplicated=false;
    }
    
    bool IsDuplicated() const {
        return duplicated;
    }
    
    void setExe_speed(double speed){
        exe_speed=speed;
    }
    
    void addSpeed(double deltaS){
        exe_speed=exe_speed+deltaS;
    }
    
    double getExe_speed() const {
        return exe_speed;
    }
    /*double getContriP(bool manybuffers, double pnf){
        if (manybuffers==true) {
            return this->getErrorRate()*w/SMAX;
        }else{
            return this->getErrorRate()*(w/exe_speed+w/SMAX+std::max(w/exe_speed,std::max(in/BETA,out/BETA))-2*pnf);
        }
    }*/
    double getvauleLs(){
        return w/exe_speed+w/SMAX+std::max(w/exe_speed,std::max(in/BETA,out/BETA));
    }
    double getSpecialS(double Pbound){
        double specialS=w/Pbound;
        
        if (discrete==true) {
            double diff;
            for (unsigned int j=0;j<Speed_Options; ++j) {
                diff=specialS-DiscreteSpeed[j];
                if (diff<=0) {
                    specialS=DiscreteSpeed[j];
                    break;
                }
            }
        }else{
            if (specialS<SMIN) {
                specialS=SMIN;
            }
        }
        
        return specialS;
    }
    void setSpecialS(double Pbound)
    {
        exe_speed=this->getSpecialS(Pbound);
    }
    double getCriticalS(double Pt){
        double CriS=w/(Pt-w/SMAX);
        
        if (discrete==true) {
            double diff;
            for (unsigned int j=0;j<Speed_Options; ++j) {
                diff=CriS-DiscreteSpeed[j];
                if (diff<=0) {
                    CriS=DiscreteSpeed[j];
                    break;
                }
            }
        }else{
            if (CriS<SMIN) {
                CriS=SMIN;
            }
        }
        
        return CriS;
    }
    void SetCriticalS(double Pt){
        exe_speed=this->getCriticalS(Pt);
    }
    void multiSpeed(double multi){
        exe_speed=speed_continuous*multi;
        speed_continuous=exe_speed;
        if (discrete==true) {
            double diff;
            for (int j=0;j<Speed_Options; ++j) {
                diff=exe_speed-DiscreteSpeed[j];
                if (diff<=0) {
                    exe_speed=DiscreteSpeed[j];
                    break;
                }
            }
        }else{
            if (exe_speed<SMIN) {
                exe_speed=SMIN;
            }
        }
    }
    double getEDerivative(){
        double EDerivative;
        //EDerivative=2*w*exe_speed-Lambda_zero*pow(w, 2)*pow(SMAX, 2)/pow(exe_speed, 2)*exp(TildeD*(SMAX-exe_speed)/(SMAX-SMIN))+Lambda_zero*pow(w, 2)*pow(SMAX, 2)*TildeD/((SMAX-SMIN)*exe_speed)*exp(TildeD*(SMAX-exe_speed)/(SMAX-SMIN));
        EDerivative=2*w*exe_speed-Lambda_zero*pow(w,2)*pow(SMAX, 2)*exp(TildeD*(SMAX-exe_speed)/(SMAX-SMIN))*(1/pow(exe_speed, 2)+TildeD/(exe_speed*(SMAX-SMIN)));
        return EDerivative;
    }
    
    void calMinESpeed(){
        double upbound=SMAX, lowbound=SMIN,cur_speed=SMIN,EDerivative,oriSp;
        
        double discriminant=Lambda_zero*w/SMAX*(1+TildeD/(SMAX-SMIN));
        if (discriminant>2) {
            minEScal=true;
            minESpeed=upbound;
            return;
        }
        
        oriSp=exe_speed;
        this->setExe_speed(lowbound);
        EDerivative=this->getEDerivative();
        
        if (EDerivative>0) {
            minESpeed=lowbound;//the task is too small, even a minimal speed is too big
            minEScal=true;
            this->setExe_speed(oriSp);
            return;
        }
        
        while (abs(EDerivative)>0.00001) {
            if (EDerivative<0) {
                cur_speed=cur_speed+(upbound-cur_speed)/2;
            }else{
                cur_speed=cur_speed-(cur_speed-lowbound)/2;
            }
            this->setExe_speed(cur_speed);
            EDerivative=this->getEDerivative();
            
            if (EDerivative<0){
                lowbound=cur_speed;
            }else{
                upbound=cur_speed;
            }
            
            if (upbound-lowbound<0.00000000001) {
                break;
            }
        }
        
        if (discrete==true) {
            int i=0;
            for (; i<Speed_Options; ++i) {
                if (DiscreteSpeed[i]>=cur_speed) {
                    minESpeed=DiscreteSpeed[i];
                    break;
                }
            }

            if (i>0) {
                double ori_speed=this->getExe_speed();
                this->setExe_speed(minESpeed);
                double energy1=pow(minESpeed, 2)+this->getErrorRate()*pow(SMAX, 2);
                this->setExe_speed(DiscreteSpeed[i-1]);
                double energy2=pow(DiscreteSpeed[i-1], 2)+this->getErrorRate()*pow(SMAX, 2);
                if (energy1>energy2) {
                    minESpeed=DiscreteSpeed[i-1];
                }
                this->setExe_speed(ori_speed);
            }
        }else{
            minESpeed=cur_speed;
        }
        
        minEScal=true;
        this->setExe_speed(oriSp);
        return;
    }
    
    void setMinESpeed(){
        if (minEScal==false) {
             this->calMinESpeed();
        }
        exe_speed=minESpeed;
    }
    
    double getMinESpeed(){
        if (minEScal==false) {
            this->calMinESpeed();
        }
        return minESpeed;
    }
    
    bool inSexc(double Pt){
        if (duplicated==true) {
            return false;
        }else if(exe_speed>=this->getCriticalS(Pt)){
            return false;
        }else{
            return true;
        }
    }
    
    void setContinuousSpeed(double Pbound){
        speed_continuous=w/Pbound;
    }
};

class Chain{
private:
    std::vector<Cnode*> * nodes;
    double energy_total=0;
    unsigned int nodes_amount;
    //unsigned int id;
public:
    Chain(int length, double *weight, double *outputdata){
        nodes = new std::vector<Cnode*>();
        
        this->AllocateNodes(length);
        
        for(int i=1;i<length+1;i++){
            Cnode * cur_node = this->getNode(i);
            cur_node->setInput(outputdata[i-1]);
            cur_node->setWeight(weight[i]);
            cur_node->setOutput(outputdata[i]);
        }
        
        this->getNode(1)->setInput(0);
        this->getNode(length)->setOutput(0);
        
        nodes_amount=length;
    }
    
    void AllocateNodes(int node_count){
        nodes->resize(node_count);
        
        for (std::vector<Cnode*>::iterator iter=nodes->begin(); iter!=nodes->end(); iter++) {
            *iter = new Cnode();
        }
    }
    
    unsigned int getLength() const{
        return nodes_amount;
    }
    
    Cnode * getNode(unsigned int node_id) const {
        return nodes->at(node_id-1);
    }
    
    ~Chain(){
        for (std::vector<Cnode*>::iterator iter=nodes->begin(); iter!=nodes->end(); iter++) {
            delete *iter;
        }
        delete nodes;
    }
    
    double getEnergy_cost_total(){
        energy_total=0;
        double energy_node=0;
            for (std::vector<Cnode*>::iterator iter=nodes->begin(); iter!=nodes->end(); ++iter) {
                energy_node=((*iter)->IsDuplicated()+1)*(*iter)->getWeight()*pow((*iter)->getExe_speed(),2)+(*iter)->getErrorRate()*(*iter)->getWeight()*pow(SMAX,2);
                /*double v1=(*iter)->getWeight();
                std::cout<<"weight: "<<v1;
                double v2=pow((*iter)->getExe_speed(),2);
                std::cout<<" square speed: "<<v2;
                double v3=(*iter)->getErrorRate();
                std::cout<<" error rate: "<<v3<<std::endl;*/
                energy_total=energy_total+energy_node;
            }
        return energy_total;
    }
    
    double getEnergyMaxS(){
        for (std::vector<Cnode*>::iterator iter=nodes->begin(); iter!=nodes->end(); ++iter) {
            (*iter)->setExe_speed(SMAX);
            (*iter)->nonDuplicate();
        }
        return getEnergy_cost_total();
    }
    
    double getPnf(){
        double largest_communication = (*std::max_element(nodes->begin(), nodes->end(), [](Cnode* i, Cnode* j){return i->getOut()<j->getOut();}))->getOut();
        double largest_computation = (*std::max_element(nodes->begin(), nodes->end(), [](Cnode* i, Cnode* j){return i->getExe_time()<j->getExe_time();}))->getExe_time();
        
        if (largest_communication>largest_computation) {
            //byCommu=true;
            return largest_communication;
        }else{
            //byCommu=false;
            return largest_computation;
        }
    }
    
    void sortbyExe_time(){
        std::sort(nodes->begin(), nodes->end(), [](Cnode* i, Cnode* j){return i->getExe_time()>j->getExe_time();});
    }
    
    void sortbyWeight(){
        std::sort(nodes->begin(), nodes->end(), [](Cnode* i, Cnode* j){return i->getWeight()>j->getWeight();});
    }
    
    void sortbyMaxEC(){
        std::sort(nodes->begin(), nodes->end(), [](Cnode* i, Cnode* j){return std::max(i->getExe_time(),i->getOut())>std::max(j->getExe_time(), j->getOut());});
    }
    
    double getLargest_commu(){
        return (*std::max_element(nodes->begin(), nodes->end(), [](Cnode* i, Cnode* j){return i->getOut()<j->getOut();}))->getOut();
    }
    
    Cnode * getSmallPossibleNode(double Pbound){
        if (discrete==false) {
            //sort nodes by a non-decreasing weight
            std::sort(nodes->begin(), nodes->end(), [](Cnode* i, Cnode* j){return i->getWeight()<j->getWeight();});
            for (std::vector<Cnode*>::iterator iter=nodes->begin(); iter!=nodes->end(); ++iter) {
                if ((*iter)->getWeight()/SMIN>=Pbound) {
                    return (*iter);
                }
            }
            return nodes->front();
        }else{
            //sort nodes by a non-increasing execution time
            std::sort(nodes->begin(), nodes->end(), [](Cnode* i, Cnode* j){return i->getExe_time()>j->getExe_time();});
            double Pnf=nodes->front()->getExe_time();
            double smallestWeight=nodes->front()->getWeight();
            Cnode* curnode=nodes->front();
            for (std::vector<Cnode*>::iterator iter=nodes->begin()+1; iter!=nodes->end(); ++iter) {
                if ((*iter)->getExe_time()==Pnf) {
                    if ((*iter)->getWeight()<smallestWeight) {
                        smallestWeight=(*iter)->getWeight();
                        curnode=(*iter);
                    }
                }else{
                    break;
                }
            }
            return curnode;
        }
    }
    
    double getProExceeds(double Pt){
        double probability=1;
        for (int i=1; i<=nodes_amount; ++i) {
            if (this->getNode(i)->getExe_time()-Pt>0.0001) {
                probability=0;
                break;
            }
            if (this->getNode(i)->inSexc(Pt)) {
                probability=probability*(1-this->getNode(i)->getErrorRate());
            }
        }
        probability=1-probability;
        return probability;
    }
};

#endif /* chains_hpp */
