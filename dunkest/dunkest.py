import pandas as pd

def group_describe_pdk(g):
    return [g.PDK.mean(), g.PDK.std(), len(g)]

def last_day_info(d, cols):
    max_day = d.Day.max()
    return d[d.Day == max_day][cols]

def build_last_day_df(df, 
                      cols,
                      groupby):
    ld = df.groupby(groupby).apply(lambda x: 
                                   last_day_info(x, cols)).reset_index()
    ld["idx"] = ["+".join([r[c] for c in groupby]) for i,r in ld.iterrows()]
    del ld[f"level_{len(groupby)}"]
    return ld

def build_pdk_df(df,
                 groupby,
                 n_days):
    df = df.groupby(groupby).tail(n_days)
    pdk = pd.DataFrame(df.groupby(groupby).apply(group_describe_pdk))
    pdk[["PDK_mean", "PDK_sd", "PDK_n"]] = list(pdk.values[:,0])
    pdk = pdk.reset_index()
    pdk["idx"] = ["+".join([r[c] for c in groupby]) for i,r in pdk.iterrows()]

    del pdk[0]
    del pdk["Player"]
    del pdk["Team"]
    del pdk["Pos"]
    return pdk

def build_pdk_cr(df,
                 ld_cols = ["CR", "PLUS", "PDK"], 
                 groupby = ["Player", "Team", "Pos"],
                 max_diff_with_current_day = 2,
                 n_days = False):
    ld_cols = ld_cols if "PDK" in ld_cols else ld_cols + ["PDK"]
    ld_cols = ld_cols if "CR" in ld_cols else ld_cols + ["CR"]

    ld = build_last_day_df(df, ld_cols, groupby)
    pdk = build_pdk_df(df, groupby, n_days)

    pdk_cr = pd.merge(ld, pdk, on = "idx")
    pdk_cr["PDK_last"] = pdk_cr.PDK.values
    del pdk_cr["PDK"]
    del pdk_cr["idx"]

    if not n_days:
        n_days = df.Day.max()

    pdk_cr = pdk_cr[pdk_cr.PDK_n <= n_days]
    pdk_cr = pdk_cr[pdk_cr.PDK_n > n_days - max_diff_with_current_day]
    pdk_cr["PDK_CR"] = pdk_cr.PDK_mean / pdk_cr.CR
    
    return pdk_cr

def pl_per_pos(d):
    pos = d.Pos.values
    u_pos = d.Pos.unique()
    return {p: len([n for n in pos if n == p]) for p in u_pos}


def build_best_team(df, prop, ascending = False, players_per_position = [4, 4, 2]):
    n_g, n_f, n_c = players_per_position
    df = df.sort_values(prop, ascending = ascending)
    g = df[df.Pos == "G"].head(n_g)
    f = df[df.Pos == "F"].head(n_f)
    c = df[df.Pos == "C"].head(n_c)

    return pd.concat([g,f,c]).round(2)