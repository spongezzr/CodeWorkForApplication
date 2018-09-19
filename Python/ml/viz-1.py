
# coding: utf-8

# # Data Visualization examples

# ## Bokeh
# 
# Install:
# 
# ```
# conda install bokeh
# ```

# In[1]:


import numpy as np
import matplotlib.pyplot as plt
from bokeh.plotting import figure, show
from bokeh.io import output_notebook

#output_notebook()

p = figure()

x = np.linspace(0,1,100)
y = np.cumsum(np.random.randn(100))

p.circle(x,y)
show(p)





