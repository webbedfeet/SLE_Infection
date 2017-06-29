# Automatically locate the data files in Dropbox

if (Sys.info()['sysname']=='Darwin') {
  info <- RJSONIO::fromJSON(
    file.path(path.expand("~"),'.dropbox','info.json'))
  dropbox_base <- info$personal$path
}
if (Sys.info()['sysname'] == 'Windows') {
  info <- RJSONIO::fromJSON(
    file.path()
  )
}
datadir <- file.path(dropbox_base, 'NIAMS','Ward','SLE_Infections')
