# -*- mode: ruby -*-
# vi: set ft=ruby :

Vagrant::Config.run do |config|
  config.vm.box = "precise64"
  config.vm.box_url = "http://files.vagrantup.com/precise64.box"
  config.vm.boot_mode = :headless
  config.vm.forward_port 3000, 3000 # Yesod

  config.vm.provision :chef_solo do |chef|
     chef.cookbooks_path = ["cookbooks"]
     chef.add_recipe "apt" # make sure apt-get update ran
     chef.add_recipe "haskell::ghc"

     chef.json = {
       :cabal => {
         :user => 'vagrant',
         :group => 'vagrant'
       }
     }
   end
end
