#!/usr/bin/env bash
scriptdir=$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )
pkill lem
pkill trayer
$scriptdir/lem.sh
