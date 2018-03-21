---
layout: default
title:  "My first blog post"
date:   2018-03-21
summary: My first jekyll post.
---

## {{ page.title }}

__4 Mar 2011 - Osaka, Japan__




## Introduction

This is my first jekyll blog post. Just to see if it works.

This is a followup post to my last entry about [LVS - Nginx - NodeJS - MongoDB - Cluster Setup on RackSpace](http://boj.github.com/blog/2011/01/14/lvs-nginx-nodejs-mongodb-cluster-setup-on-rackspace/), and focuses on implementing iptables rules to lock down the cluster from both external and internal probing.  It's important to note that RackSpace's servers are wide open on all interfaces, and that other members in their internal network can sniff ports on your machine's internal interfaces.

I am by no means an iptables expert, and while I have used the tool for many reasons over the years it has never been with complete comprehension as to how it works beyond a superficial level.  Therefore, if anything in regards to my settings below seem to be off, strange, or makes no sense, please feel free to leave advice in the comments section below.  I am publishing this for myself as much as the next guy who needs a few hints about setting up iptables rules for things like LVS-TUN or RackSpace.
