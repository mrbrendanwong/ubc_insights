# Deliverable 1 Plan
### Final Due Date: October the 24th

## Breakdown Of Each Milestone:

### Insight Facade
- Implement InsightFacade in controller/InsightFacade.ts
- Import code from RouteHandler
- Write unit tests for methods
##### Other Concerns:
##### Tests:

### GROUP
- Seperate from WHERE, works along with APPLY
- Creates GROUP for every unique set of N-terms
- Appears with APPLY
##### Other Concerns:
- How much does GET logic change?
##### Tests:
- If we group by dept, do we get one of each dept?
- If we group by course ID, do we get one of each course?
- Not very practical to group by avg
- In general, check if we get one of every specific group

### APPLY
- We can now "GET" things that are "APPLY"'D
- Will display chosen results from each GROUP
##### Other Concerns:
- How much does GET logic change?
##### Tests:
- Are we grabbing the correct max?
- For basic cases, let's say groups of course_id, we can check numeric fields for MAX and actual to see if they match

### ORDER
- The query now has "dir" and "keys" objects within them
- "dir is either UP or DOWN"
##### Other Concerns:
- Does case of UP or DOWN matter
##### Tests:
- Trivial

## Priorities
1. Insight Facade
2. GROUP/APPLY
3. ORDER

## Time Allocation
This is the approximate work schedule we be following on a weekly basis
* Thursday: 4 hr
* Friday: 4 hr
* Satuday: 2 hr
* Sunday: 2 hr
* TOTAL EXPECTED WORK TIME: 12 hr
