#!/usr/bin/env python
# coding: utf-8

# In[1]:


import pandas as pd
import numpy as np
from river import drift


# In[2]:


df = pd.read_csv('//home/lucas/bfd.csv', index_col=0)
df


# In[4]:


adwin = drift.ADWIN()
# Simulating a data stream as a normal distribution of 1's and 0's
# Adding stream elements to ADWIN and verifying if drift occurred
for i in df['depart_visibility']:
    adwin.update(i)    
    if adwin.drift_detected:
        print('Change detected in data: ' + str(i) + ' - at index: ' + str(i))


# In[ ]:


adwin

