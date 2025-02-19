# TODO: Implement specification: https://bids-specification.readthedocs.io/en/stable/modality-agnostic-files.html#phenotypic-and-assessment-data

# Phenotypic and assessment data
#
# Template:
#
#   phenotype/
#   <measurement_tool_name>.tsv
#   <measurement_tool_name>.json
#
# Optional: Yes
#
# If the dataset includes multiple sets of participant level measurements (for example responses from multiple questionnaires) they can be split into individual files separate from participants.tsv.
#
# Each of the measurement files MUST be kept in a /phenotype directory placed at the root of the BIDS dataset and MUST end with the .tsv extension. Filenames SHOULD be chosen to reflect the contents of the file. For example, the "Adult ADHD Clinical Diagnostic Scale" could be saved in a file called /phenotype/acds_adult.tsv.
#
# The files can include an arbitrary set of columns, but one of them MUST be participant_id and the entries of that column MUST correspond to the subjects in the BIDS dataset and participants.tsv file.
#
# As with all other tabular data, the additional phenotypic information files MAY be accompanied by a JSON file describing the columns in detail (see Tabular files).
#
# In addition to the column descriptions, the JSON file MAY contain the following fields:
#   Key name 	Requirement Level 	Data type 	Description
# MeasurementToolMetadata 	OPTIONAL 	object 	A description of the measurement tool as a whole. Contains two fields: "Description" and "TermURL". "Description" is a free text description of the measurement tool. "TermURL" is a URL to an entity in an ontology corresponding to this tool.
# Derivative 	OPTIONAL 	boolean 	Indicates that values in the corresponding column are transformations of values from other columns (for example a summary score based on a subset of items in a questionnaire).
#
# Must be one of: "true", "false".
#
# As an example, consider the contents of a file called phenotype/acds_adult.json:
#
#   {
#     "MeasurementToolMetadata": {
#       "Description": "Adult ADHD Clinical Diagnostic Scale V1.2",
#       "TermURL": "https://www.cognitiveatlas.org/task/id/trm_5586ff878155d"
#     },
#     "adhd_b": {
#       "Description": "B. CHILDHOOD ONSET OF ADHD (PRIOR TO AGE 7)",
#       "Levels": {
#         "1": "YES",
#         "2": "NO"
#       }
#     },
#     "adhd_c_dx": {
#       "Description": "As child met A, B, C, D, E and F diagnostic criteria",
#       "Levels": {
#         "1": "YES",
#         "2": "NO"
#       }
#     }
#   }
#
# Please note that in this example MeasurementToolMetadata includes information about the questionnaire and adhd_b and adhd_c_dx correspond to individual columns.
#
# In addition to the keys available to describe columns in all tabular files (LongName, Description, Levels, Units, and TermURL) the participants.json file as well as phenotypic files can also include column descriptions with a Derivative field that, when set to true, indicates that values in the corresponding column is a transformation of values from other columns (for example a summary score based on a subset of items in a questionnaire).



