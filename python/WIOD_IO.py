import numpy as np
import pandas as pd
import math


class parse_WIOD:
    a_country = ["FIN"]
    EU_countries = [
        "AUT",
        "BEL",
        "BGR",
        "CYP",
        "CZE",
        "DEU",
        "DNK",
        "EST",
        "ESP",
        "FIN",
        "FRA",
        "GRC",
        "HRV",
        "HUN",
        "IRL",
        "ITA",
        "LTU",
        "LUX",
        "LVA",
        "MLT",
        "NLD",
        "POL",
        "PRT",
        "ROU",
        "SWE",
        "SVN",
        "SVK",
    ]
    ################################################################
    A = ["A01", "A02", "A03"]
    B = ["B"]
    C = [
        "C10-C12",
        "C13-C15",
        "C16",
        "C17",
        "C18",
        "C20",
        "C21",
        "C22",
        "C23",
        "C24",
        "C25",
        "C26",
        "C27",
        "C28",
        "C31_C32",
        "C33",
    ]
    C19 = ["C19"]
    C29 = ["C29", "C30"]
    D = ["D35"]
    E = ["E36", "E37-E39"]
    F = ["F"]
    G = ["G45", "G46", "G47"]
    H = ["H49", "H50", "H51"]
    H_M_N = ["H52", "H53", "M69_M70", "M71", "M72", "M73", "M74_M75", "N"]
    I = ["I"]
    J_R_S = ["J58", "J59_J60", "J61", "J62_J63", "R_S"]
    K = ["K64", "K65", "K66"]
    L = ["L68"]
    O = ["O84"]
    P = ["P85"]
    Q = ["Q"]
    T_U = ["T"]

    agg_ind = {
        "A": A,
        "B": B,
        "C": C,
        "C19": C19,
        "C29": C29,
        "D": D,
        "E": E,
        "F": F,
        "G": G,
        "H": H,
        "H_M_N": H_M_N,
        "I": I,
        "J_R_S": J_R_S,
        "K": K,
        "L": L,
        "O": O,
        "P": P,
        "Q": Q,
        "T": T_U,
    }

    @classmethod
    def set_class_indices(cls, IO):
        cls.sectors = list(IO.index.get_level_values(0).unique())
        cls.regions = list(IO.index.get_level_values(1).unique())
        ################################################################
        cls.y_cat = [
            x
            for x in IO.columns.get_level_values(0).unique()
            if x not in cls.sectors + ["(industry-by-industry)", "GO"]
        ]
        cls.multindex = pd.MultiIndex.from_product([cls.regions, cls.sectors])
        cls.non_EU_countries = [x for x in cls.regions if x not in cls.EU_countries]
        cls.other_countries = [x for x in cls.regions if x not in cls.a_country]

    def __init__(self, IO):
        self.base_WIOD(IO)

    def base_WIOD(self, IO):
        IO.drop(
            labels=[
                ("(industry-by-industry)", "Unnamed: 1_level_1"),
                ("(industry-by-industry)", "Unnamed: 3_level_1"),
                ("GO", "TOT"),
            ],
            axis=1,
            inplace=True,
        )
        IO.columns.names = ["sector", "region"]
        IO.index.names = ["sector", "region"]
        IO = IO.swaplevel(i=0, j=1, axis=1)
        IO = IO.swaplevel(i=0, j=1, axis=0)
        # some of the columns are 0 -> int64, cast as float64 to avoid that and issues later
        self.Z = IO.loc[:, (self.regions, self.sectors)].astype("float64")
        self.x = IO.sum(axis=1).squeeze()
        x_inv = 1 / self.x
        x_inv.fillna(value=0, inplace=True)
        x_inv.replace([np.inf, -np.inf], 0, inplace=True)
        self.x_inv_diag = np.diag(x_inv)
        self.A = self.Z @ self.x_inv_diag
        self.A.columns = self.A.index
        self.y = IO.loc[(self.regions, self.sectors), (self.regions, self.y_cat)]
        self.tot_y = self.y.sum(axis=1)
        self.L = np.linalg.inv((np.identity(len(self.A)) - self.A).to_numpy())
        self.calc_country_IO()

    def calc_country_IO(self):
        self.Z_U = self.Z[self.a_country].groupby(level=1, sort=False).sum()
        self.Z_dom = self.Z.loc[self.a_country, self.a_country]
        self.Z_U.columns = self.Z_U.columns.droplevel(level=0)
        self.Z_dom.columns = self.Z_dom.columns.droplevel(level=0)
        self.Z_dom.index = self.Z_dom.index.droplevel(level=0)
        self.Z_imp = self.Z_U - self.Z_dom
        self.y_domestic = self.y.loc[self.a_country, self.a_country]
        self.y_imports = (
            self.y.loc[self.other_countries, self.a_country]
            .groupby(level=1, sort=False)
            .sum()
        )
        self.y_exports = (
            self.y.loc[self.a_country, self.other_countries]
            .T.groupby(level=1, sort=False)
            .sum()
            .T
        )
        self.z_exports = (
            self.Z.loc[self.a_country, self.other_countries]
            .T.groupby(level=1, sort=False)
            .sum()
            .T
        )
