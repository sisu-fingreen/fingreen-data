import numpy as np
import pandas as pd
import math

class parse_WIOD:
    dom=['ITA']
    ################################################################
    A=['A01','A02','A03']
    B=['B']
    C=['C10-C12', 'C13-C15', 'C16', 'C17', 'C18', 'C20', 'C21', 'C22', 'C23', 'C24', 'C25', 'C26', 'C27', 'C28', 'C31_C32', 'C33']
    C19=['C19']
    C29=['C29','C30']
    D=['D35']
    E=['E36','E37-E39']
    F=['F']
    G=['G45', 'G46','G47']
    H=['H49', 'H50', 'H51']
    H_M_N=['H52', 'H53','M69_M70', 'M71', 'M72', 'M73','M74_M75','N']
    I=['I']
    J_R_S=['J58', 'J59_J60', 'J61','J62_J63','R_S']
    K=['K64', 'K65', 'K66']
    L=['L68']
    O=['O84']
    P=['P85']
    Q=['Q']
    T_U=['T']
    agg_ind={'A':A,'B':B,'C':C,'C19':C19,'C29':C29,'D':D,'E':E,'F':F,'G':G,'H':H,'I':I,'J_R_S':J_R_S,'K':K,'L':L,'H_M_N':H_M_N,'O':O,'P':P,'Q':Q,'T':T_U}

    @classmethod
    def set_class_indices(cls,IO):
        cls.sectors=list(IO.index.get_level_values(0).unique())
        cls.regions=list(IO.index.get_level_values(1).unique())
        cls.y_cat=[x for x in IO.columns.get_level_values(0).unique() if x not in cls.sectors+['(industry-by-industry)','GO']]
        cls.mulindex=pd.MultiIndex.from_product([cls.regions,cls.sectors])
        cls.oth=[x for x in cls.regions if x not in cls.dom]


        

    def __init__(self,IO):
        self.base_WIOD(IO)

    def base_WIOD(self,IO):
        IO.drop(labels=[('(industry-by-industry)', 'Unnamed: 1_level_1'),('(industry-by-industry)', 'Unnamed: 3_level_1'),('GO','TOT')],axis=1,inplace=True)
        IO.columns.names=['sector','region']
        IO.index.names=['sector','region']
        IO=IO.swaplevel(i=0,j=1,axis=1)
        IO=IO.swaplevel(i=0,j=1,axis=0)     
        self.Z=IO.loc[(self.regions,self.sectors),(self.regions,self.sectors)]
        self.x=IO.loc[(self.regions,self.sectors),:].sum(axis=1).squeeze()
        self.Z_dom=self.Z.loc[self.dom,self.dom].droplevel(level=0,axis=1).droplevel(level=0,axis=0)
        self.Z_imp=self.Z.loc[self.oth,self.dom].groupby(level=1,sort=False).sum().droplevel(level=0,axis=1)
        self.x_dom=self.x.loc[self.dom].droplevel(level=0)
        self.y=IO.loc[(self.regions,self.sectors),(self.regions,self.y_cat)]
        self.Z_exp=self.Z.loc[self.dom,self.oth].groupby(axis=1,level=1,sort=False).sum().droplevel(level=0,axis=0)

class parse_KLEMS:
    dom=['IT']
    ################################################################
    A=['A']
    B=['B']
    C=['C10-C12', 'C13-C15', 'C16-C18', 'C20', 'C21', 'C22-C23', 'C24-C25', 'C26', 'C27', 'C28', 'C31-C33']
    C19=['C19']
    C29=['C29-C30']
    D=['D']
    E=['E']
    F=['F']
    G=['G']
    H=['H']
    H_M_N=['M-N']
    I=['I']
    J_R_S=['J','R-S']
    K=['K']
    L=['L']
    O=['O']
    P=['P']
    Q=['Q']
    T_U=['T']
    agg_ind={'A':A,'B':B,'C':C,'C19':C19,'C29':C29,'D':D,'E':E,'F':F,'G':G,'H':H,'I':I,'J_R_S':J_R_S,'K':K,'L':L,'H_M_N':H_M_N,'O':O,'P':P,'Q':Q,'T':T_U}

