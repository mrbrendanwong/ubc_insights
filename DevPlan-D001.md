# Deliverable 1 Plan

## Final Due Date: October the 2nd

## Breakdown Of Each Milestone:

1. GET / Rest endpoint
..* No modification required

2. PUT /dataset/:id endpoint
..* Parse info from zip file
..* Decide data structure to store parsed data
..* Make sure data persists; don't just read from files
..* Error handling: work on response codes

Other Concerns:
Robustify file input system: Only accept specified file type

Tests:
* Can we put new data?
* Can we put data that was cached?
* Can we put same data twice in a session?

3. POST /query endpoint
..* Interpreting requests
..* Grab corresponding data depending on id provided
..* Check persisting data
..* Error handling: work on response codes

Other Concerns:
Tests:
* Can we query nonexistent data?
* Can we query cached data?
* Can we query data that was just PUT
* Do we recieve correct error codes?

4. DELETE /dataset/:id endpoint
..* Ensure file path references and data on disk and memory are deleted
..* Error handling: work on response codes

Other Concerns:
Tests:
* Can we delete same dataset twice?
* Can we delete nonexistent dataset?
* If we delete, are we able to query?

## Priorities

## Time Allocation
This is the approximate work schedule we be following on a weekly basis
Monday: 2 hr
Tuesday: 2 hr
Wednesday: 2 hr
Thursday: 4 hr
Friday: 1 hr
Satuday: 1-2 hr
Sunday: N/A
TOTAL WEEKLY WORK TIME: 12 hr


PUT /dataset/:id allows to submit a zip file that will be parsed and used for future queries. The zip file content will be sent in base64 in the PUT body.
The dataset will be in a format designed to be transferred, not a format amenable to easy searching. After receiving the dataset, it should be processed into a data structure of your design.
The processed data structure should be persisted to disk; your system should be able to load this persisted value into memory for answering queries.
Ultimately, a dataset must be PUT or loaded from disk before queries can be successfully answered.
Response Codes and message formats:
204: the operation was successful and the id was new (not PUT in this session or was previously cached).
201: the operation was successful and the id already existed (was PUT in this session or was previously cached).
400: the operation failed. The body should contain {error: 'my text'} to explain what went wrong.

POST /query sends the query to the application. The query will be in JSON format in the post body.
Query term prefixes (e.g. courses) will correspond to the dataset required by the query. These prefixes correspond exactly to the id in the PUT endpoint.
NOTE: the server may be shutdown between the PUT and the POST. This endpoint should always check for a persisted data structure on disk before returning a missing dataset error.
Response Codes and message formats:
200: the query was successfully answered. The result should be sent in JSON according in the response body.
424: the query failed because it depends on a resource that has not been PUT. The body should contain {missing: ['key1'...]}.
400: the query failed; body should contain {error: 'my text'} providing extra detail.

DELETE /dataset/:id deletes the existing dataset stored.
This will delete both disk and memory caches for the dataset for the id meaning that subsequent queries for that id should fail unless a new PUT happens first.
Response Codes and message formats:
204: the operation was successful.
404: the operation was unsuccessful because the delete was for a resource that was not previously PUT.