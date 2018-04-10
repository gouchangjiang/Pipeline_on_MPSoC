//
//  utils.cpp
//  Workflows
//
//  Created by changjiang GOU on 07/12/2017.
//  Copyright © 2017 Changjiang GOU. All rights reserved.
//

#include "utils.hpp"
#include <fstream>
#include "iostream"
#include <cstring>

void parse_tree(const char *filename,int * N ,double **nwghts,double **ewghts){
    std::ifstream OpenFile(filename);
    char begin;
    char cur_char;
    int line_index = 0;
    bool nodes_cnt_read = false;
    unsigned int nb_of_nodes=0;
    std::string line;
    
    do
    {
        begin = OpenFile.peek();
        if(OpenFile.good() ){
            if(begin=='%'){
                do{
                    cur_char = OpenFile.get();
                }while(cur_char != '\n' && OpenFile.good());
            }
            else{
                if(!nodes_cnt_read){
                    /* get the number of nodes and skip last trailing character*/
                    while(getline(OpenFile,line)){++nb_of_nodes;}
                    OpenFile.clear();
                    OpenFile.seekg(0,std::ios::beg);
                    *N = nb_of_nodes;
                    
                    nodes_cnt_read = true;
                    /*allocate space for nodes*/
                    *nwghts = new double[nb_of_nodes+1];
                    *ewghts = new double[nb_of_nodes+1];
                }
                else{
                    /*parse actual nodes*/
                    //unsigned int id;
                    //unsigned int parent;
                    double ew, nw;
                    
                    OpenFile >> nw >> ew;
                    //std::cout<<"nw: "<<nw<<" ew: "<<ew<<std::endl;
                    do{cur_char = OpenFile.get();}while(cur_char != '\n' && OpenFile.good());
                    //parent = nb_of_nodes - parent + 1;
                    /*(*prnts)[line_index] = parent;
                     (*nwghts)[line_index] = nw;
                     (*ewghts)[line_index] = ew;*/
                    (*nwghts)[nb_of_nodes-line_index] = nw;
                    (*ewghts)[nb_of_nodes-line_index] = ew;
                    
                    line_index++;
                }
            }
        }
    }while(OpenFile.good() );
    OpenFile.close();
}
