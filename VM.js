//FIXME: separate this virtual machine model from the view

var noCPUs = 3;
var memSize = 4;

var refCnts = new Array(noData);
var usedMem = 0;

var totalLoads = 0;

// initialize reference count to 0
for (var i = 0; i < noData; i++)
    refCnts[i] = 0;

function updateMem() {
    // first clear memory
    for (var i = 0; i < memSize; i++) {
        var memid = "#mem" + i;
        $(memid).html("");
    }

    // then update each entry
    usedMem = 0;
    for (var i = 0; i < noData; i++) {
        if (refCnts[i] > 0) {
            var memid = "#mem" + usedMem;
            $(memid).html(i);
            usedMem++;
        }
    }
}

function loadData(i,j) {
    // calculate needed memory
    need = refCnts[i] > 0 ? 1 : 0;
    need += refCnts[j] > 0 ? 1 : 0;

    // quit if memory is not available
    if (usedMem+need > memSize)
        return false;

    // load data, i.e., increase reference count
    refCnts[i]++;
    refCnts[j]++;

    // set no loads
    totalLoads += need;
    $("#newLoads").html("+" + need);
    $("#totalLoads").html(totalLoads);

    return true;
}

function freeData(i,j) {
    // making sure that reference counts are always >= 0
    refCnts[i] = refCnts[i] <= 1 ? 0 : refCnts[i]-1;
    refCnts[j] = refCnts[j] <= 1 ? 0 : refCnts[j]-1;
}

var usedCPUs = 0;
var lastCPU = 0;
var CPUs = new Array(noCPUs);

function startJob(i,j) {
    // check if processor is available, and if so, load data
    if (usedCPUs < noCPUs) {
        var cpu = (lastCPU + usedCPUs) % noCPUs;

        // set cpu in mem
        CPUs[cpu] = [i,j];

        // show job being executed
        var cpuid = "#cpu" + cpu;
        $(cpuid).html(i + "," + j);

        usedCPUs++;
        return true;
    }

    return false;
}

function stopLastJob() {
    if (usedCPUs <= 0)
        return false;

    // free up memory
    var cpu = lastCPU;
    freeData(CPUs[cpu][0], CPUs[cpu][1]);

    var cpuid = "#cpu" + cpu;
    $(cpuid).html("");

    lastCPU = (lastCPU+1) % noCPUs;
    usedCPUs--;

    // if no jobs are running, next job should be launched cpu 0
    //if (usedCPUs == 0)
    //    lastCPU = 0;

    return true;
}

