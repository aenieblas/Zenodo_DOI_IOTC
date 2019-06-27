############### read function ###############
## insert sharelink into iotc_config.json file
make_json_step1<-function(sharelink,token){
  cat('{
  "id": "Zenodo_publish_workflow",
      "profile": {
      "project": "Test geoflow project",
      "organization": "My organization"
      },
      "mode": "entity",
      "dependencies": {
      "packages": {
      "cran": [
      "gsheet"
      ],
      "cran_force_install" : false,
      "github": [],
      "github_force_install": false
      },
      "scripts": []
      },
      "metadata": {
      "entities": {
      "handler": "gsheet",
      "source": "',sharelink,'"
      }
      },
      "software": [
      {
      "id": "my-zenodo",
      "type": "output",
      "software_type": "zenodo",
      "parameters": {
      "url": "https://sandbox.zenodo.org/api",
      "token": "',token,'"
      },
      "properties" : {
      "clean": true
      }
      }
      ],
      "actions": [
      {
      "id": "zen4R-deposit-record",
      "options": {
      "publish": false
      },
      "run": true
      }
      ]
}',file='iotc_config_step1.json',sep='')
}
# ########################################################
# 
# ##
# "handler": "handle_entities_gsheet_iotc",
# "script": "geoflow_handler_entity_iotc.R",