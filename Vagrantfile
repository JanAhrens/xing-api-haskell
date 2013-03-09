# -*- mode: ruby -*-
# vi: set ft=ruby :

Vagrant::Config.run do |config|
  config.vm.box       = 'precise64'
  config.vm.box_url   = 'http://files.vagrantup.com/precise64.box'

  config.vm.boot_mode = :headless
  config.vm.customize ['modifyvm', :id, '--memory', 1024]

  config.vm.forward_port 3000, 3000 # Yesod

  config.vm.provision :chef_solo do |chef|
     chef.cookbooks_path = ['cookbooks']

     chef.add_recipe 'apt'
     chef.add_recipe 'build-essential'
     chef.add_recipe 'vim'
     chef.add_recipe 'git'
     chef.add_recipe 'haskell'

     chef.json = {
       # configuration of the haskell cookbook
       :travis_build_environment => {
         :user  => 'vagrant',
         :group => 'vagrant',
         :home  => '/home/vagrant'
       }
     }
   end
end
