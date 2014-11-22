VAGRANT_DIR='/home/chris/Vagrant/DROP-001_Local'

cd $VAGRANT_DIR
vagrant up

cd -
ansible-playbook deploy.yml

cd $VAGRANT_DIR
vagrant halt
