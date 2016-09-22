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

1. PUT (Parse, interpret and store the data)
2. DELETE (deleting current dataset)
3. POST (Querying the current dataset)

## Time Allocation
This is the approximate work schedule we be following on a weekly basis
* Monday: 2 hr
* Tuesday: 2 hr
* Wednesday: 2 hr
* Thursday: 4 hr
* Friday: 1 hr
* Satuday: 1-2 hr
* Sunday: N/A
* TOTAL WEEKLY WORK TIME: 12 hr