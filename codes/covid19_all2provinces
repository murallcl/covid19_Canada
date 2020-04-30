# -*- coding: utf-8 -*-
"""
Created on Sat Apr 11 13:41:45 2020

@author: Masih M. Saber (morteza.mahmoudisaber@umontreal.ca)
Dependencies: Python3, pandas

command line:
python covid19_all2province.py --exl <input_excel_file> --exl_out <output excel file or complete path>
"""
import pandas as pd
import argparse

def get_options():
    

    description = 'Extract COVID19 data for each province in Canada'
    parser = argparse.ArgumentParser(description=description)

    parser.add_argument('--exl',
                        help='Excel file containing COVID19 data')
    parser.add_argument('--exl_out',
                        help='output name/path')

    return parser.parse_args()

options = get_options()


class COVID:
    """ Extract COVID19 data for each province in Canada

    Parameters
    ----------
    excel : excel file including COVID19 data
    
    Returns
    ----------
    exl_out : excel file
    Excel file with data for each province at a single spreadshit.

    """
    def __init__(self,exl):
        self.all=pd.read_excel(exl, sheet_name='Cases', encoding = 'unicode_escape',skiprows=3)
        self.mort=pd.read_excel(exl,sheet_name='Mortality', encoding = 'unicode_escape',skiprows=3)
        self.rec=pd.read_excel(exl,sheet_name='Recovered', encoding = 'unicode_escape',skiprows=3)
        
        
    def reader(self):
        al=self.all
        al['date_report'] = al['date_report'].dt.date
        mort=self.mort
        mort['date_death_report'] = mort['date_death_report'].dt.date
        #mort.rename(columns={'date_death_report':'date_report'}, inplace=True)
        rec=self.rec
        rec['date_recovered']=rec['date_recovered'].dt.date
        #rec.rename(columns={'date_recovered':'date_report'}, inplace=True)
        dat_dict={}
        dat_dict['CANADA']={}
        prov=al['province'].unique()
        for prov_ in prov:
            dat_dict[prov_]={}
            al_df=al[al['province']==prov_]
            mort_df=mort[mort['province']==prov_]
            rec_df=rec[rec['province']==prov_]
            cum_date= pd.concat([al_df['date_report'], mort_df['date_death_report'], rec_df['date_recovered']])
            for date_ in cum_date.sort_values().unique():
                al_case=al_df[al_df['date_report']==date_].shape[0]
                al_death=mort_df[mort_df['date_death_report']==date_].shape[0]
                recs_=list(rec_df[rec_df['date_recovered']==date_]['cumulative_recovered'])
                if len (recs_)> 0:
                    al_rec=recs_[0]
                else:
                    al_rec=0
                if al_case>0 or al_death>0 or al_rec>0:
                    dat_dict[prov_][date_]=[al_case,al_death,al_rec]
                    if date_ in dat_dict['CANADA']:
                        dat_dict['CANADA'][date_][0]+=al_case
                        dat_dict['CANADA'][date_][1]+=al_death
                        dat_dict['CANADA'][date_][2]+=al_rec
                    else:
                        dat_dict['CANADA'][date_]=[al_case,al_death,al_rec]
                        
                        
        return(dat_dict)
        
    def transform(self,exl_out):
        dat=self.reader()
        exl_main = pd.ExcelWriter(exl_out) 
        for prov_ in dat:
            dat_=[]
            cas_=[]
            mort_=[]
            rec_=[]
            for date_ in dat[prov_]:
                dat_.append(date_)
                cas_.append(dat[prov_][date_][0])
                mort_.append(dat[prov_][date_][1])
                rec_.append(dat[prov_][date_][2])
            df_=pd.DataFrame(
                    {'date':dat_,
                     'case':cas_,
                     'death':mort_,
                     'recovered':rec_}
                    )
            df_.to_excel(exl_main,sheet_name=prov_)
        exl_main.save()
            
                
        
df=COVID(options.exl)
df.transform(options.exl_out)
        
#df=COVID('C:\\Users\\morte\\OneDrive\\projects\\covid19\\Public_COVID-19_Canada.xlsx')
#df.transform('C:\\Users\\morte\\OneDrive\\projects\\covid19\\covid19_reformated.xlsx')
