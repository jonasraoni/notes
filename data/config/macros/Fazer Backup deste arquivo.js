
if (tabs.getTabType() == 'file'){
  os.fileDelete(tabs.getFileName() + '~');
  os.fileCopy(tabs.getFileName(), tabs.getFileName() + '~');
  if (os.fileExists(tabs.getFileName() + '~') == true){

       alert('Um backup foi criado em ' + tabs.getFileName() + '~');
  }
}






