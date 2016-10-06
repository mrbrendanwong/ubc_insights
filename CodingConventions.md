# CODING CONVENTIONS:
This is a self-made guideline for keeping our comments and logging clean and comprehensive. It is not required, but will make our lives easier.

## Logging to console:
* Whether we do a `Log.trace()` or a `console.log()`, specify what class and method we are logging within.
```javascript
Log.trace('RouteHandler::postQuery: ...');
console.log('RouteHandler.postQuery: ...');
```

* Provide a description for the purpose of the logging.
```javascript
console.log('DatasetController.process: processing is done');
```

* If we are outputting any sort of data to the logging, ensure that a description of what the data actually is is provided.
```javascript
console.log('DatasetController.process: returning dataset[id]' + this.dataset[id]);
``` 

* Console messages should only be kept for development and debugging purposes. Please remove them after the code has been sufficiently tested or if deemed unecessary for the completed function.

## Comments:
* If we are commenting out default code, note that it is default code and not something we wrote
```javascript.
// DEFAULT CODE: let result = controller.query(query);
```

* If you are currently working on any sort of method, and you have to push with it incomplete, tag it with a TODO and a description of what is left to complete.
```javascript
// TODO: initial attempt at delete method.
public static deleteDataset() {...}
```

* If we are commenting out our own code, note that it is code written by you by mentioning your name in all caps and describe what the commented out code is. These comments may be removed ater your partner has reviewed your code or if deemed not needed for the completed function.
```javascript
public static deleteDataset() {
    // BY BRENDON: prototype for delete.
    // Log.trace(...)
    // try {
    //     ...
    // } catch {
    //     ...
    // }
}
```

* If you create any new method, mention that it was created by you and provide a short description of what it does. These comments may be removed after your partner reviewed your code or on your next commmit.
```javascript
// BY SPENCER: cheecks for dataset on disk or in cache. deletes if found in either.
public static deleteDataset() {...}
```

## Safety Checks
* Run tsc before each commit to check for any typescript errors.
* Regularly run tslint to ensure we are following ts style conventions.