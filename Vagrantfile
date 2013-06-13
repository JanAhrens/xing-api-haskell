# -*- mode: ruby -*-
# vi: set ft=ruby :

Vagrant.configure("2") do |config|
  config.vm.box       = 'precise64'
  config.vm.box_url   = 'http://files.vagrantup.com/precise64.box'

  config.vm.provider "virtualbox" do |v|
    v.gui = false
    v.customize ['modifyvm', :id, '--memory', 1024]
  end

  config.vm.network :forwarded_port, guest: 3000, host: 3000 # Yesod

  config.vm.provision :chef_solo do |chef|
    # reminder: add cookbook dependencies to the Berksfile
    chef.add_recipe 'apt'
    chef.add_recipe 'build-essential'
    chef.add_recipe 'vim'
    chef.add_recipe 'git'
    chef.add_recipe 'haskell::source'

    chef.json = {
      # configuration of the haskell cookbook
      :travis_build_environment => {
        :user  => 'vagrant',
        :group => 'vagrant',
        :home  => '/home/vagrant'
      },
      :ghc => {
        :version => '7.6.3'
      },
      :haskell => {
        :platform => {
          :version => '2013.2.0.0'
        }
      }
    }
  end
end
