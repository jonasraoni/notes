
// returna true se o valor (string) está no objeto
function isIn(avalue, aobject){
    var aham= false;
    var val= String(avalue).toLowerCase();
    for(var i in aobject){
        if (String(aobject[i]).toLowerCase() == val) {
          aham= true;
          break;
        }
    }
    return aham;
}

function sortAndRemoveDuplicates(unsortedArray){
    var a = new Array();
    for (var i in unsortedArray){
        if (!isIn(unsortedArray[i], a)){
            a[a.length]= unsortedArray[i];
        }
    }
    a.sort();
    return a;
}

// Completação "comum" de palavra
function WordComplete(){
    notes.exec('actEditorSelWord');
    var s= editor.getSelText();
    editor.setSelStart( editor.getSelStart() + editor.getSelLength() );
    if (s.length > 0){
        var reg= new RegExp("\\b" + s + "\\w+\\b", "gi");
        var txt= editor.getText();
        var words = txt.match(reg);
        if (words != null && words != 'undefined'){
            words= sortAndRemoveDuplicates(words);
            dlgs.codeCompletition(words.join('\n'));
        }
    }
}

WordComplete();


