import databento as db
path="/Users/simongu/Documents/Library_QPA/GLBX-20251118-NQPHUN6869/glbx-mdp3-20100606-20251116.ohlcv-1m.dbn"
stored_data=db.DBNStore.from_file(path)
# Convert to dataframe
df = stored_data.to_df()
df.reset_index(inplace=True)

# 3. NOW when you save or print, ts_event is there
#save to csv
df.to_csv("glbx-mdp3-20100606-20251116.ohlcv-1m.csv", index=False)