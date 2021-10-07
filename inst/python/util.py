import json
import pandas as pd
import plotly
import sktime.classification.compose

def convert_sensors(df, deployid_col, datetime_col, sensor_cols):
  """Convert R sensor representation to Python.
  
  Converts an R data.frame containing multiple deployments to a Python dict of
  DataFrames, keyed by deployment.
  
  Args:
    df (DataFrame): sensor data in tabular format.
    deployid_col (str): name of column with deployment ID.
    datetime_col (str): name of column with datetime.
    sensor_cols (List[str]): name of columns with sensor data.
  
  Returns:
    Dict of sensor DataFrames, compliant with Stickleback.
  """
  sensors = dict()
  for deployid in df[deployid_col].unique():
    sensors[deployid] = (df.loc[df[deployid_col] == deployid]
                         .set_index(datetime_col)
                         .drop(deployid_col, axis=1)
                         .loc[:, sensor_cols])
        
  return sensors

def convert_events(df, deployid_col, datetime_col):
  """Convert R sensor representation to Python.
  
  Converts an R data.frame containing times of labeled behavioral events to a 
  Python dict of DatetimeIndexes, keyed by deployment.
  
  Args:
    df (DataFrame): event data in tabular format.
    deployid_col (str): name of column with deployment ID.
    datetime_col (str): name of column with datetime.
  
  Returns:
    Dict of event DatetimeIndexes, compliant with Stickleback.
  """
  events = dict()
  for deployid in df[deployid_col].unique():
    datetime = df[df[deployid_col] == deployid][datetime_col]
    events[deployid] = pd.DatetimeIndex(datetime)
        
  return events

def timedelta(timedelta_str):
  """Create a Timedelta object.
  
  Args:
    timedelta_str (str): string representation of Timedelta, e.g. "5H".
    
  Returns:
    Timedelta object.
  """
  return pd.Timedelta(timedelta_str)

def datetimeindex_to_isoformat(df):
  """Convert DataFrame's DatetimeIndex to string column in ISO format.
  
  Args:
    df (DataFrame): DataFrame indexed by DatetimeIndex.
    
  Returns:
    As df, but times from index in a column (ISO format).
  """
  return (df
          .reset_index()
          .rename(columns={'index':'datetime'}))
          
def plotly_to_json(fig):
  return json.dumps(fig, cls=plotly.utils.PlotlyJSONEncoder)

def compose_tsc(tsc, cols):
  return sktime.classification.compose.ColumnEnsembleClassifier(
    estimators = [('est_{}'.format(col), tsc, [i])
                  for i, col in enumerate(cols)]
  )
