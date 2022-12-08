# Test & Run

```sh
# Run tests in part1.odin
$ odin test part1.odin -file

# Execute part1.odin with input from input.txt
$ odin run part1.odin -file -- input.txt
```

# Using Vagrant

The Odin compiler doesn't seem to work on openSUSE Tumbleweed,
so Vagrant is set up with an Ubuntu box if it's needed.

```sh
$ sudo service libvirtd start
$ sudo service nfs-server start
$ vagrant up
```

Once the box is running, you can access it with

```sh
$ vagrant ssh
$ cd /advent
```
