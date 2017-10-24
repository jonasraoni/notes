
var regex= new RegExp('');
var hassupport = true;

switch (editor.getFileType().toLowerCase()){
    case 'c-c++' :
        regex.compile('^[a-zA-Z_]+[^;\\(\\)]*[a-zA-Z_]+\\([^;\\n/\\{]*', 'gm');
        break;
    case 'delphi' :
        regex.compile('^[ \\t]*(function|procedure|constructor|destructor)[ \\t].*[a-zA-Z_]+[^;\\n/\\{]*', 'gim');
        break;
    case 'perl' :
        regex.compile('^[ \\t]*sub[ \\t][^;\\n\\]*', 'gm');
        break;
    case 'php' :
        regex.compile('^[ \\t]*function[ \\t].*\\([^;\\n/\\{]*', 'gm');
        break;
    case 'javascript' :
        regex.compile('^[ \\t]*function[ \\t].*\\([^;\\n/\\{]*', 'gm');
        break;
    case 'java' :
        regex.compile('^[ \\t]*(public|protected|private|static)[ \\t].*\\([^;\\n/\\{]*', 'gm');
        break;
    case 'jsp' :
        regex.compile('^[ \\t]*(public|protected|private|static)[ \\t].*\\([^;\\n/\\{]*', 'gm');
        break;
    case 'c#' :
        regex.compile('^[ \\t]*(public|protected|private|static)[ \\t].*\\([^;\\n/\\{]*', 'gm');
        break;
    case 'asp' :
        regex.compile('^[ \\t]*(public|private)?[ \\t]*(sub|function|property|class)[ \\t].*', 'gim');
        break;
    case 'visualbasic' :
        regex.compile('^[ \\t]*(public|private)?[ \\t]*(sub|function|property|class)[ \\t].*[a-zA-Z_]+[^\\n]*', 'gim');
        break;
    default : hassupport = false;
}

if (hassupport){

    var txt= String(editor.getText());
    var funcs = txt.match(regex);

    if (funcs != null && funcs != 'undefined')
    {
        var s= dlgs.list('Function list... ', funcs.join('\n'));

        if (s != ''){
            var pos= txt.lastIndexOf(s);
            if (pos > 0){
                editor.setSelStart(pos-1);
            }
        }

    } else {

        notes.status('No functions found, sorry :(');
    }

} else {
    dlgs.error(editor.getFileType() + ' is not suported yet.\nBut you can add suport editing this script :)');
}


