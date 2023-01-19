#!/bin/sh

setup_git() {
  git config --global user.email "git@github.com"
  git config --global user.name "Github Actions CI"
}

clone_flusight_repo() {
  echo "Cloning repo..."
  git clone https://github.com/lshandross/Flusight-forecast-data.git
  echo "cloned flusight repo"
}

setup_git
clone_flusight_repo