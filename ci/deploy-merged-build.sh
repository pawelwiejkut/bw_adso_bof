#!/bin/bash

# Build merged
npm run merge || exit 1
wc -l ./zbw_adso_bof.abap

# Deploy artifacts
git clone https://github.com/pawelwiejkut/bw_adso_bof.git
cp zbw_adso_bof.abap bw_trfn_tester/last_build/zbw_adso_bof.abap
cd zbw_adso_bof

# Commit
git status
git config user.email "ci@pawelwiejkut.net"
git config user.name "CI"
git add last_build/zbw_adso_bof

.abap
git commit -m "CI build [skip ci]" || exit 1
git push -q https://$GITHUB_API_KEY@github.com/pawelwiejkut/bw_adso_bof.git 
