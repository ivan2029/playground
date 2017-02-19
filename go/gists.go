package main

import (
	"net/http"
	"io/ioutil"
	"encoding/json"
)

type Files map[string]interface{}

type GistInfo struct {
	Url    string  `json:"url"`
	Files  Files   `json:"files"`
}

type GistInfos []GistInfo

type Gist struct {
	Files Files `json:"files"`
}

func main() {
	resp, err := http.Get("https://api.github.com/users/ivan2029/gists")
	checkError(err)
	defer resp.Body.Close()

	body, err := ioutil.ReadAll(resp.Body)
	checkError(err)
	
	var gists GistInfos
	err = json.Unmarshal(body, &gists)

	for _, gistInfo := range gists {
		resp, err = http.Get(gistInfo.Url)
		checkError(err)
		defer resp.Body.Close()
			
		body, err = ioutil.ReadAll(resp.Body)
		checkError(err)

		var gist Gist
		err = json.Unmarshal(body, &gist)
		checkError(err)
		
		for file, fileInfo := range gist.Files {
			switch fileInfo.(type) {
			case map[string]interface{}:
				m := fileInfo.(map[string]interface{})
				content := m["content"]
				err = ioutil.WriteFile(file, []byte(content.(string)), 0666)
				checkError(err)
			}
		}
	}
}

func checkError(err error) {
	if err != nil {
		panic(err.Error())
	}
}
