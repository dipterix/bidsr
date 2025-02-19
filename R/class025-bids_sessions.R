# https://bids-specification.readthedocs.io/en/stable/modality-agnostic-files.html#sessions-file
#
# Sessions file
#
# Template:
#
#   sub-<label>/
#     sub-<label>_sessions.tsv
#
# Optional: Yes
#
# In case of multiple sessions there is an option of adding additional sessions.tsv files describing variables changing between sessions. In such case one file per participant SHOULD be added. These files MUST include a session_id column and describe each session by one and only one row. Column names in sessions.tsv files MUST be different from group level participant key column names in the participants.tsv file.
# Column name 	Requirement Level 	Data type 	Description
# session_id 	REQUIRED 	string 	A session identifier of the form ses-<label>, matching a session found in the dataset. There MUST be exactly one row for each session.
#
# Values in session_id MUST be unique.
#
# This column must appear first in the file.
# acq_time 	OPTIONAL 	string 	Acquisition time refers to when the first data point of the first run was acquired. Datetime format and their deidentification are described in Units.
#
# This column may appear anywhere in the file.
# pathology 	RECOMMENDED 	string or number 	String value describing the pathology of the sample or type of control. When different from healthy, pathology SHOULD be specified. The pathology may be specified in either samples.tsv or sessions.tsv, depending on whether the pathology changes over time.
#
# This column may appear anywhere in the file.
# Additional Columns 	OPTIONAL 	n/a 	Additional columns are allowed.
#
# _sessions.tsv example:
#
#   session_id  acq_time    systolic_blood_pressure
# ses-predrug 2009-06-15T13:45:30 120
# ses-postdrug    2009-06-16T13:45:30 100
# ses-followup    2009-06-17T13:45:30 110


