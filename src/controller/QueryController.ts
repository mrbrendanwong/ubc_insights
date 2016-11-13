/**
 * Created by rtholmes on 2016-06-19.
 */

import {Datasets} from "./DatasetController";
import Log from "../Util";
import DatasetController from '../controller/DatasetController';

export interface QueryRequest {
    GET: string|string[];
    WHERE: {};
    ORDER: any;
    AS: string;
    GROUP: string[];
    APPLY: any[];
    add?: number[];
    multiply?: number[];
}

export interface QueryResponse {
}

export default class QueryController {
    private static datasetController = new DatasetController();
    private datasets:Datasets = null;

    constructor(datasets:Datasets) {
        this.datasets = datasets;
    }

    public isValid(query:QueryRequest):boolean {


        // Don't provide anything, return false
        if (query == null)
            return false;

        if (query.GET == undefined || query.GET == null || query.AS == undefined || query.AS == null)
            return false;

        // Group needs to contain at least 1 element, return false
        if (query.GROUP != undefined && query.GROUP.length == 0)
            return false;

        // Empty Group && non-empty Apply
        if ((query.APPLY != undefined) && (query.GROUP == undefined
            || (query.GROUP != undefined && query.GROUP.length == 0)))
            return false;

        // Non-Empty group and empty Apply
        if ((query.GROUP != undefined) && (query.APPLY == undefined))
            return false;

        // D1 Order key not in Get
        if (typeof query.ORDER === 'string') {
            if (query.GET.indexOf(query.ORDER) < 0)
                return false;
        }

        // D2 Order key not in Get
        if (typeof query.ORDER === 'object') {
            let orderKeys = query.ORDER.keys;
            for (var i = 0; i < orderKeys.length; i++) {
                if (query.GET.indexOf(orderKeys[i]) < 0)
                    return false;
            }
        }

        // Special handling for Group and Apply
        if (query.GROUP != undefined && (query.APPLY != undefined)) {
            return this.applyGroupValidation(query);
        }
        // Should catch whatever made it through to this point
        if (typeof query !== 'undefined' && query !== null && Object.keys(query).length > 0) {
            console.log('QueryController.isValid: Object.keys(query) is ' + Object.keys(query));
            return true;
        }

        return false;
    }

    private applyGroupValidation(query:any):boolean {
        let matchFlag:boolean = false;
        let secondaryMatchFlag:boolean = false;

        // Check if each apply element is unique
        for (var i = 0; i < query.APPLY.length; i++) {
            if (i == 0)
                continue;
            if (Object.keys(query.APPLY[i-1])[0] == Object.keys(query.APPLY[i])[0]) {
                console.log("APPLY keys not all unique");
                return false;
            }
        }

        // Check if all keys in group are in GET
        for (var q = 0; q < query.GET.length; q++) {
            matchFlag = false;
            if (query.GET[q].indexOf("_") >= 0) {
                for (var w = 0; w < query.GROUP.length; w++) {
                    if (query.GET[q] == query.GROUP[w]) {
                        matchFlag = true;
                    }
                }
                if (!matchFlag) {
                    return false;
                }
            }
        }
        // Make sure no "_" in Group keys not sure what second or-statement does
        for (var x = 0; x < query.GROUP.length; x++) {
            if (query.GROUP[x].indexOf("_") == -1) {
                console.log("Invalid key in Group");
                return false;
            }
        }


        //Makes sure any non "_" keys in GET are in APPLY
        for (var y = 0; y < query.GET.length; y++) {
            // Find non-"_" Keys
            matchFlag = false;
            if (query.GET[y].indexOf("_") == -1) {
                for (var z = 0; z < query.APPLY.length; z++) {
                    if (query.GET[y] == Object.keys(query.APPLY[z])[0])
                        matchFlag = true;
                }
                if (!matchFlag)
                    return false;
            }
        }

        // Make sure key does not appear in both APPLY and GROUP
        for (var j = 0; j < query.GROUP.length; j++){
            for (var k = 0; k < query.APPLY.length; k++){
                let outmostKey:any = query.APPLY[k];
                let outerKey:any = Object.keys(query.APPLY[k])[0];
                let innerKey:any = Object.keys(outmostKey[Object.keys(outmostKey)[0]])[0];
                let desiredID = outmostKey[outerKey][innerKey];
                if ((innerKey == 'MAX') || (innerKey == 'MIN') || (innerKey == 'AVG')) {
                    if ((desiredID != "courses_avg") && (desiredID != "courses_pass") &&  (desiredID != "courses_fail") && (desiredID != "courses_audit"))
                        return false;
                }
                if (query.GROUP[j] == desiredID) {
                    return false;
                }
            }
        }

        // Check if GET key is contained in either GROUP or APPLY
        var applyKeys:any[] = [];
        for (var t = 0; t < query.APPLY.length; t++) {
            applyKeys[t] = Object.keys(query.APPLY[t])[0];
        }


        for (var e = 0; e < query.GROUP.length; e++) {
            matchFlag = false;
            for (var r = 0; r < query.GET.length; r++) {
                if (query.GET[r] == query.GROUP[e])
                    matchFlag = true;
            }
            if (!matchFlag)
                return false;
        }

        for (var d = 0; d < query.APPLY.length; d++) {
            secondaryMatchFlag = false;
            for (var s = 0; s < query.GET.length; s++) {
                if (query.GET[s] == applyKeys[d])
                    secondaryMatchFlag = true;
            }
            if (!secondaryMatchFlag)
                return false;
        }
        return true;
    }

    private queryWhere(whereRequests:any, getRequests:any, rawData:Array<any>, notFlag:boolean, dataset1:Array<any> = [], dataset2:Array<any> = []):any {
        let whereID:Array<string>;
        let restriction:Array<string>;
        if (whereRequests.length == 0)
            return rawData;
        else {
            switch (Object.keys(whereRequests)[0]) {
                case 'GT':
                    restriction = Object.keys(whereRequests.GT);
                    whereID = whereRequests.GT[restriction.toString()];
                    return this.processWhere(rawData, 'GT', restriction, whereID, notFlag, getRequests);
                case 'LT':
                    restriction = Object.keys(whereRequests.LT);
                    whereID = whereRequests.LT[restriction.toString()];
                    return this.processWhere(rawData, 'LT', restriction, whereID, notFlag, getRequests);
                case 'EQ':
                    restriction = Object.keys(whereRequests.EQ);
                    whereID = whereRequests.EQ[restriction.toString()];
                    return this.processWhere(rawData, 'EQ', restriction, whereID, notFlag, getRequests);
                case 'IS':
                    restriction = Object.keys(whereRequests['IS']);
                    whereID = whereRequests['IS'][restriction.toString()];
                    return this.processWhere(rawData, 'IS', restriction, whereID, notFlag, getRequests);
                case 'AND':
                    var previousData = rawData;
                    var currentData:any;
                    for (var i = 0; i < whereRequests.AND.length; i++) {
                        currentData = this.queryWhere(whereRequests.AND[i], getRequests, previousData, notFlag);
                        previousData = currentData;
                    }
                    return currentData;
                case 'OR':
                    var datasetArray:any = [];
                    for (var i = 0; i < whereRequests.OR.length; i++) {
                        dataset1 = this.queryWhere(whereRequests.OR[i], getRequests, rawData, notFlag);
                        datasetArray.push(dataset1);
                    }
                    var oldDataset = {};
                    for (var z = 0; z < datasetArray.length; z++) {
                        if (z == 0) {
                            oldDataset = datasetArray[z];
                        } else {
                            oldDataset = this.unionArrays(oldDataset, datasetArray[z], getRequests);
                        }
                    }
                    return oldDataset;
                case 'NOT':
                    return this.queryWhere(whereRequests.NOT, getRequests, rawData, !notFlag);
                default:
                    console.log("Unsupported WHERE request");
                    return null;
            }
        }

    }

    // http://codegolf.stackexchange.com/questions/17127/array-merge-without-duplicates
    private unionArrays(a1:any, a2:any, getRequests:any):any {
        var hash:any = {};
        var arr:any = [];
        for (var i = 0; i < a1.length; i++) {
            if (hash[a1[i]["courses_uuid"]] !== a1[i]["courses_uuid"])
                hash[a1[i]["courses_uuid"]] = a1[i]["courses_uuid"];
            arr[arr.length] = a1[i];
        }
        for (var i = 0; i < a2.length; i++) {
            if (hash[a2[i]["courses_uuid"]] !== a2[i]["courses_uuid"]) {
                hash[a2[i]["courses_uuid"]] = a2[i]["courses_uuid"];
                arr[arr.length] = a2[i];
            }
        }
        return arr;
    }

    private processWhere(data:Array<any>, whereCondition:string, restriction:any, restrictionValue:any, notFlag:boolean = false, getRequests:any):any {
        let processedData:Array<any> = [];
        switch (whereCondition) {
            case 'GT':
                if (notFlag) {
                    for (var i = 0; i < data.length; i++) {
                        if (data[i][restriction] <= restrictionValue)
                            processedData.push(data[i]);
                    }
                } else {
                    for (var i = 0; i < data.length; i++) {
                        if (data[i][restriction] > restrictionValue) {
                            processedData.push(data[i]);
                        }
                    }
                }
                break;
            case 'LT':
                if (notFlag) {
                    for (var i = 0; i < data.length; i++) {
                        if (data[i][restriction] >= restrictionValue)
                            processedData.push(data[i]);
                    }
                } else {
                    for (var i = 0; i < data.length; i++) {
                        if (data[i][restriction] < restrictionValue)
                            processedData.push(data[i]);
                    }
                }
                break;
            case 'EQ':
                if (notFlag) {
                    for (var i = 0; i < data.length; i++) {
                        if (data[i][restriction] !== restrictionValue) {
                            processedData.push(data[i]);
                        }
                    }
                } else {
                    for (var i = 0; i < data.length; i++) {
                        if (data[i][restriction] === restrictionValue) {
                            processedData.push(data[i]);
                        }
                    }
                }
                break;
            case 'IS':
                if (notFlag) {
                    for (var i = 0; i < data.length; i++) {
                        if ((restrictionValue.localeCompare(data[i][restriction])) != 0)
                            processedData.push(data[i]);
                    }
                } else {
                    if (restrictionValue[restrictionValue.length - 1] == "*" && restrictionValue[0] != "*") {
                        restrictionValue = restrictionValue.replace(/\*/g, '');
                        for (var i = 0; i < data.length; i++) {
                            if ((data[i][restriction]).startsWith(restrictionValue)) {
                                processedData.push(data[i]);
                            }
                        }
                    } else if (restrictionValue[0] == "*" && restrictionValue[restrictionValue.length - 1] != "*") {
                        restrictionValue = restrictionValue.replace(/\*/g, '');
                        for (var i = 0; i < data.length; i++) {
                            if ((data[i][restriction]).endsWith(restrictionValue)) {
                                processedData.push(data[i]);
                            }
                        }
                    } else if (restrictionValue[0] == "*" && restrictionValue[restrictionValue.length - 1] == "*") {
                        restrictionValue = restrictionValue.replace(/\*/g, '');
                        for (var i = 0; i < data.length; i++) {
                            if ((data[i][restriction]).indexOf(restrictionValue) >= 0) {
                                processedData.push(data[i]);
                            }
                        }
                    } else {
                        for (var i = 0; i < data.length; i++) {
                            if (restrictionValue == (data[i][restriction]))
                                processedData.push(data[i]);
                        }
                    }
                }
                break;
            default:
                console.log("Attempting to process unsupported WHERE query");
                break;
        }
        // call this later
        //  var filteredData = this.filterByGET(processedData, getRequests);
        return processedData;
    }

    private filterByGET(unfinishedDataset:any, getRequests:any, applyRequests:any = []):any {
        var finalizedArray:any = [];
        for (var x = 0; x < unfinishedDataset.length; x++) {
            var currentResult:any = {};
            for (var z = 0; z < getRequests.length; z++) {
                let datasetID = getRequests[z].split("_")[0];
                let dataID = getRequests[z].split("_")[1];
                switch (datasetID) {
                    case 'courses':
                        switch (dataID) {
                            case 'dept':
                                currentResult["courses_dept"] = unfinishedDataset[x].courses_dept;
                                break;
                            case 'id':
                                currentResult["courses_id"] = unfinishedDataset[x].courses_id;
                                break;
                            case 'avg':
                                currentResult["courses_avg"] = unfinishedDataset[x].courses_avg;
                                break;
                            case 'instructor':
                                currentResult["courses_instructor"] = unfinishedDataset[x].courses_instructor;
                                break;
                            case 'title':
                                currentResult["courses_title"] = unfinishedDataset[x].courses_title;
                                break;
                            case 'pass':
                                currentResult["courses_pass"] = unfinishedDataset[x].courses_pass;
                                break;
                            case 'fail':
                                currentResult["courses_fail"] = unfinishedDataset[x].courses_fail;
                                break;
                            case 'audit':
                                currentResult["courses_audit"] = unfinishedDataset[x].courses_audit;
                                break;
                            case 'uuid':
                                currentResult["courses_uuid"] = unfinishedDataset[x].courses_uuid;
                                break;
                            default:
                                console.log("Uh oh, you sent an invalid key");
                                break;
                        }
                        break;
                    case 'rooms':
                        switch (dataID) {
                            case 'fullname':
                                currentResult["rooms_fullname"] = unfinishedDataset[x].rooms_fullname;
                                break;
                            case 'shortname':
                                currentResult["rooms_shortname"] = unfinishedDataset[x].rooms_shortname;
                                break;
                            case 'number':
                                currentResult["rooms_number"] = unfinishedDataset[x].rooms_number;
                                break;
                            case 'name':
                                currentResult["rooms_name"] = unfinishedDataset[x].rooms_name;
                                break;
                            case 'address':
                                currentResult["rooms_address"] = unfinishedDataset[x].rooms_address;
                                break;
                            case 'lat':
                                currentResult["rooms_lat"] = unfinishedDataset[x].rooms_lat;
                                break;
                            case 'lon':
                                currentResult["rooms_lon"] = unfinishedDataset[x].rooms_lons;
                                break;
                            case 'seats':
                                currentResult["rooms_seats"] = unfinishedDataset[x].rooms_seats;
                                break;
                            case 'type':
                                currentResult["rooms_type"] = unfinishedDataset[x].rooms_type;
                                break;
                            case 'furniture':
                                currentResult["rooms_furniture"] = unfinishedDataset[x].rooms_furniture;
                                break;
                            case 'href':
                                currentResult["rooms_href"] = unfinishedDataset[x].rooms_href;
                                break;
                            default:
                                console.log("Uh oh, you sent an invalid key");
                                break;
                        }
                        break;

                    default:
                        break;
                }
            }
            finalizedArray.push(currentResult);
        }
        return finalizedArray;
    }

// UP means lowest first, DOWN means highest first
    private queryOrder(query: QueryRequest, unsortedData: Array<any>, originalSort: boolean): any {
        var orderKeys: any;
        var downDir: boolean = false;
        if (query.ORDER == undefined) {
            orderKeys = "courses_dept";
        } else if (originalSort) {
            orderKeys ="courses_uuid";
        } else if (typeof query.ORDER === 'string') {
            orderKeys = query.ORDER;
        } else if (typeof query.ORDER === 'object') {
            console.log(query.ORDER['dir']);
            if (query.ORDER['dir'] == 'DOWN')
                downDir = true;
            orderKeys = query.ORDER.keys;
            console.log(orderKeys);
        }

        var sortedData = unsortedData.sort(
            function comparator(a,b): any {
                var property: any;
                if (typeof orderKeys === 'string')
                    property = orderKeys;
                else
                    property = orderKeys[0];
                if (a[property] < b[property]) return -1;
                if (a[property] > b[property]) return 1;
                if (a[property] == b[property]){
                    for (var i = 1; i < orderKeys.length; i++) {
                        if (a[orderKeys[i]] < b[orderKeys[i]]) return -1;
                        if (a[orderKeys[i]] > b[orderKeys[i]]) return 1;
                        if (a[orderKeys[i]] == b[orderKeys[i]]) continue;
                    }
                    return 0;
                }
            });
        if (downDir)
            sortedData = sortedData.reverse();

        return sortedData;
    }

    private queryAs(query:QueryRequest, resultArray:Array<any>):any {
        console.log(query.AS);
        if (resultArray == null)
            return null;
        if (query.AS === "TABLE") {
            var dataObject:any = {};
            dataObject['render'] = "TABLE";
            dataObject['result'] = resultArray;
        }
        return dataObject;
    }


    private queryGroup(groupRequests:any, dataset:any):any {

        let groupedDataset:any = [];
        let groupQualities:any;
        // For every offering

        var hash:any = {};

        for (var x = 0; x < dataset.length; x++) {
            // collect the necessary info from current iteration
            groupQualities = [];
            for (var y = 0; y < groupRequests.length; y++){
                groupQualities.push(dataset[x][groupRequests[y]]);
            }
            if (hash[groupQualities] !== undefined)
                continue;
            else {
                hash[groupQualities] = [];
            }
        }

        // Goes back through dataset and adds the data to the appropriate spot in the hash
        for (var z = 0; z < dataset.length; z++) {
            groupQualities = [];
            for (var w = 0; w < groupRequests.length; w++){
                groupQualities.push(dataset[z][groupRequests[w]]);
            }
            if (hash[groupQualities] !== undefined)
                hash[groupQualities].push(dataset[z]);
            else
                continue;
        }

        // Return data to original form
        Object.keys(hash).forEach(function(key,index) {
            groupedDataset.push(hash[key]);
        });

        return groupedDataset;
    }

    // Verifies if two course offerings should be grouped together
    private shouldBeGrouped(offering1:any, offering2:any, groupRequests:any):boolean {
        let groupWorthy:boolean = true;
        for (var x = 0; x < groupRequests.length; x++) {
            if (offering1[groupRequests[x]] != offering2[groupRequests[x]])
                groupWorthy = false;
        }
        return groupWorthy;
    }


    private queryApply(query:any, applyRequests:any, groupRequests:any, groupedDataset:any):any {
        // Go through each set of applications
        let appliedDataset:any = [];
        for (var x = 0; x < groupedDataset.length; x++) {
            appliedDataset.push(this.applyComputations(query, applyRequests, groupRequests, groupedDataset[x]));
            // Send each group to the computation helper
        }
        return appliedDataset;

    }

    private applyComputations(query:any, applyKeys:any, groupRequests:any, dataInstance:any):any {
        // datainstance is an array of offerings (corresponding to a group)
        // order back to OG form
        let computatedObject:any = {};
        let desiredID:any = "";
        for (var i = 0; i < query.GET.length; i++) {
            computatedObject[query.GET[i]] = dataInstance[0][query.GET[i]];
        }
        for (var z = 0; z < applyKeys.length; z++) {
            let trueApplyKey:any = applyKeys[z];

            let applicationID:any = Object.keys(applyKeys[z])[0];
            let finalApplyKey:any = Object.keys(trueApplyKey[Object.keys(trueApplyKey)[0]])[0];

            if (applyKeys.length == 0)
                return dataInstance;
            switch (finalApplyKey) {
                case 'AVG':
                    let sum = 0;
                    let count = dataInstance.length - 1;

                    desiredID = trueApplyKey[applicationID][finalApplyKey];
                    if (typeof dataInstance[0][desiredID] !== 'number')
                        break;
                    for (var x = 0; x < dataInstance.length; x++) {
                        sum += dataInstance[x][desiredID];
                    }
                    let result = sum / x;
                    computatedObject[applicationID] = Number(result.toFixed(2));
                    break;
                case 'COUNT':
                    // figure something out
                    let uniqueArray:any = [];
                    desiredID = trueApplyKey[applicationID][finalApplyKey];
                    for (var q = 0; q < dataInstance.length; q++){
                        if (q == 0)
                            uniqueArray.push(dataInstance[q][desiredID]);
                        else {
                            for (var t = 0; t < uniqueArray.length; t++){
                                if (dataInstance[q][desiredID] == uniqueArray[t]) {
                                    break;
                                }
                                else if (uniqueArray[t+1] == undefined || uniqueArray[t+1] == null)
                                    uniqueArray.push(dataInstance[q][desiredID]);
                            }
                        }
                    }
                    computatedObject[applicationID] = uniqueArray.length;
                    break;
                case 'MAX':
                    let maxValue = -10000;
                    desiredID = trueApplyKey[applicationID][finalApplyKey];
                    if (typeof dataInstance[0][desiredID] !== 'number')
                        break;
                    for (var x = 0; x < dataInstance.length; x++) {
                        if (dataInstance[x][desiredID] > maxValue)
                            maxValue = dataInstance[x][desiredID];
                    }
                    computatedObject[applicationID] = maxValue;
                    break;
                case 'MIN':
                    let minValue = 1000000000;
                    desiredID = trueApplyKey[applicationID][finalApplyKey];
                    if (typeof dataInstance[0][desiredID] !== 'number')
                        break;
                    for (var x = 0; x < dataInstance.length; x++) {
                        if (dataInstance[x][desiredID] < minValue)
                            minValue = dataInstance[x][desiredID];
                    }
                    computatedObject[applicationID] = minValue;
                    break;
                default:
                    console.log("Uh oh, this method received an unsupported APPLY key");
                    break;
            }
        }
        return computatedObject;
    }

    // Pushes 'pushElement' to 'targetArray' if array does not already contain the element
    public noDuplicatePush(pushElement: any, targetArray: any[]): any[] {
        if (targetArray.indexOf(pushElement) < 0)
            targetArray.push(pushElement);
        return targetArray;
    }

    // Pushes each element in 'sourceArray' to 'targetArray' if target array does not already contain the element
    public noDuplicateConcat(sourceArray: any[], targetArray: any[]): any[] {
        for (var i = 0; i < sourceArray.length; i++) {
            if (targetArray.indexOf(sourceArray[i]) < 0)
                targetArray.push(sourceArray[i]);
        }
        return targetArray;
    }

    // TODO: switch array adding to no duplicate push
    public applyKeyExtraction(queryApply: any): any[] {
        let applyKeys: any[] = [];
        if (queryApply != undefined || queryApply != null) {
            for (var i = 0; i < queryApply.length; i++)
                applyKeys[i] = Object.keys(queryApply[i])[0];
        }
        return applyKeys;
    }

    // Extracts all resource keys from WHERE
    public whereKeyExtraction(queryWhere: any): any {
        let whereKeys: any[] = [];

        if (queryWhere == {})
            return whereKeys;
        else {
            let whereKey: any;
            switch (Object.keys(queryWhere)[0]) {
                case 'GT':
                    whereKey = Object.keys(queryWhere['GT']).toString();
                    whereKeys.push(whereKey);
                    break;
                case 'LT':
                    whereKey = Object.keys(queryWhere['LT']).toString();
                    whereKeys.push(whereKey);
                    break;
                case 'EQ':
                    whereKey = Object.keys(queryWhere['EQ']).toString();
                    whereKeys.push(whereKey);
                    break;
                case 'IS':
                    whereKey = Object.keys(queryWhere['IS']).toString();
                    whereKeys.push(whereKey);
                    break;
                case 'AND':
                    for (var i = 0; i < queryWhere['AND'].length; i++) {
                        let deepKeys = this.whereKeyExtraction(queryWhere['AND'][i]);
                        whereKeys = this.noDuplicateConcat(deepKeys, whereKeys);
                    }
                    break;
                case 'OR':
                    for (var i = 0; i < queryWhere['OR'].length; i++) {
                        let deepKeys = this.whereKeyExtraction(queryWhere['OR'][i]);
                        whereKeys = this.noDuplicateConcat(deepKeys, whereKeys);
                    }
                    break;
                case 'NOT':
                    let deepKeys = this.whereKeyExtraction(queryWhere['NOT']);
                    whereKeys = this.noDuplicateConcat(deepKeys, whereKeys);
                    break;
                default:
                    return whereKeys;
            }
            console.log("These are whereKeys " + whereKeys);
            return whereKeys;
        }
    }

    private handleEmptyApply(dataset:any):any {
        let handledArray:any = [];
        for (var i = 0; i < dataset.length; i++) {
            handledArray.push(dataset[i][0]);
        }
        return handledArray;
    }


    private queryD1(query:QueryRequest, queryResult:any): QueryResponse {
        let completedWhereQuery:any;
        let completedOrderQuery:any;
        let dataset1:Array<any> = [];
        let dataset2:Array<any> = [];
        var resultToBeRendered:any;
        var filteredData:any;
        if (query.WHERE) {
            completedWhereQuery = this.queryWhere(query.WHERE, query.GET, queryResult, false, dataset1, dataset2);
            filteredData = this.filterByGET(completedWhereQuery, query.GET);
        }else
            filteredData = this.filterByGET(queryResult, query.GET);

        completedOrderQuery = this.queryOrder(query, filteredData, false);
        resultToBeRendered = this.queryAs(query, completedOrderQuery);
        return resultToBeRendered;
    }

    private queryD2(query:QueryRequest, queryResult:any): QueryResponse {
        let completedWhereQuery:any;
        let completedOrderQuery:any;
        let completedGroupQuery:any;
        let completedApplyQuery:any;
        let dataset1:Array<any> = [];
        let dataset2:Array<any> = [];
        let emptyApplyQuery:any = [];
        var resultToBeRendered:any;
        var filteredData:any;

        if (Object.keys(query.WHERE).length != 0) {
            completedWhereQuery = this.queryWhere(query.WHERE, query.GET, queryResult, false, dataset1, dataset2);
            completedGroupQuery = this.queryGroup(query.GROUP, completedWhereQuery);
        } else {
            completedGroupQuery = this.queryGroup(query.GROUP, queryResult);
        }
        if (query.APPLY.length != 0) {
            completedApplyQuery = this.queryApply(query, query.APPLY, query.GROUP, completedGroupQuery);
            completedOrderQuery = this.queryOrder(query, completedApplyQuery, false); //why was this changed
        } else {
            emptyApplyQuery = this.handleEmptyApply(completedGroupQuery);
            filteredData = this.filterByGET(emptyApplyQuery, query.GET);
            completedOrderQuery = this.queryOrder(query, filteredData, false);
        }
        resultToBeRendered = this.queryAs(query, completedOrderQuery);
        return resultToBeRendered;
    }

    public query(query:QueryRequest):QueryResponse {
        Log.trace('QueryController::query( ' + JSON.stringify(query) + ' )');
        // TODO: implement this (where we handle get, where, etc.)
        let queryResult:Array<any>;
        let controller = QueryController.datasetController;



        if (query.GET) {
            // #D1 support
            queryResult = controller.queryDataset(query.GET);
            if (query.GROUP == undefined || query.GROUP.length == 0)
                return this.queryD1(query, queryResult);
            else
                return this.queryD2(query, queryResult);

        }
    }
}