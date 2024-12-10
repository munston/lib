to use this with store a local cabal project file should exist (cabal.project)
in a directory containing dir "src" with the store and lib dirs from the github repos in.
with lines eg;

packages: 
 src/lib
 src/store  

package lib
package store
