import requests
import json
import snowflake.connector
from datetime import datetime, timedelta
from airflow import DAG
from airflow.hooks.base_hook import BaseHook
from airflow.operators.python_operator import PythonOperator
from airflow.sensors.s3_key_sensor import S3KeySensor
from airflow.operators.slack_operator import SlackAPIPostOperator

# dates
today = datetime.today()
yesterdate = today + timedelta(days = -1)
yesterday = yesterdate.strftime('%Y-%m-%d')

# credentials
database_name = 'MY_DATABASE'
table_name = 'MY_TABLE'
bucket_name = '<my_bucket_name>'
bucket_key = '<my_bucket_key>'
snowflake_username = BaseHook.get_connection('snowflake').login
snowflake_password = BaseHook.get_connection('snowflake').password
snowflake_account = BaseHook.get_connection('snowflake').host
aws_s3_access_key_id = BaseHook.get_connection('aws_s3_access_key_id').password
aws_s3_secret_access_key = BaseHook.get_connection('aws_s3_secret_access_key').password
slack_username = BaseHook.get_connection('slack').login
slack_token = BaseHook.get_connection('slack').password
slack_user_code = '<my_slack_user_code>'

def upload_to_snowflake():
    
    con = snowflake.connector.connect(
                                      user = snowflake_username
                                      , password = snowflake_password
                                      , account = snowflake_account
                                      )
    cs = con.cursor()
    
    cs.execute('USE DATABASE %s;' % database_name)
    
    copy = (
            "COPY into %s"
            " from s3://%s/%s"
            " credentials = (aws_key_id = '%s' aws_secret_key = '%s')"
            " file_format = (type = csv field_delimiter = ','"
            " field_optionally_enclosed_by = '\"'"
            " skip_header = 1)"
            " on_error = 'continue' force = True;"
            % (
               table_name
               , bucket_name
               , bucket_key
               , aws_s3_access_key_id
               , aws_s3_secret_access_key
               )
            )
            
    cs.execute(copy)
    
    cs.close()

def completion_slack_message():
    message = (
            ":white_check_mark: *Daily Update* \n"
            "*Date*: %s \n"
            "*Warehouse*: SNOWFLAKE \n"
            "*Database*: %s \n"
            "*Table*: %s \n"
            "*Alert*: `Successfully uploaded`"
            % (
               yesterday
               , database_name
               , table_name
               )
            )
    
    post = {"text": "{0}".format(message)}
 
    url = BaseHook.get_connection('slack').login
    
    requests.post(
            url
            , data = json.dumps(post)
            , headers = {'Content-Type': 'application/json'}
        )

def failure_slack_message(context):
    
    slack_alert = SlackAPIPostOperator(
        task_id = 'task_failure_slack_message',
        channel = "#data_notifications",
        token = slack_token,
        username = slack_username
        text = (
                ":heavy_exclamation_mark: *Daily Update* \n"
                "*Date*: %s \n"
                "*Alert*: `DAG: %s, Task: %s`"
                % (
                   yesterday
                   , context.get('task_instance').dag_id
                   , context.get('task_instance').task_id
                   )
                )
            )
    
    return(slack_alert.execute(context=context))

DAG_DEFAULT_ARGS = {
        'owner': 'airflow',
        'depends_on_past': False,
        'start_date': datetime.utcnow(),
        'retries': 1,
        'retry_delay': timedelta(minutes=5)
}

with DAG(dag_id = "s3_snowflake_slack_pipeline", 
         default_args = DAG_DEFAULT_ARGS,
         schedule_interval = "0 8 * * *", 
         catchup = False
         ) as dag:
    
    file_sensor = S3KeySensor(task_id = 's3_key_sensor_task',
                              poke_interval = 60 * 30, 
                              timeout = 60 * 60 * 12, 
                              bucket_key = "s3://%s/%s" % (),
                              bucket_name = None,
                              wildcard_match = False,
                              on_failure_callback = failure_slack_message,
                              dag = dag)
    
    upload_file = PythonOperator(task_id = "upload_to_snowflake_task",
                                 python_callable = upload_to_snowflake,
                                 on_failure_callback = failure_slack_message,
                                 dag = dag)
    
    completion_slack_message = PythonOperator(task_id = "completion_slack_message_task",
                                              python_callable = completion_slack_message,
                                              on_failure_callback = failure_slack_message,
                                              dag = dag)
    
    file_sensor >> upload_file >> completion_slack_message
