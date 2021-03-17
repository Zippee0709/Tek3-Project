import axios from 'axios'
const apiKey = 'Your api key'

export async function search(query)
{
    const url = `https://api.nytimes.com/svc/search/v2/articlesearch.json?q="${query}"&${apiKey}`;
    const res = await axios.get(url);
    if (res.status !== 200) {
        console.error("Error with the test API");
        return null;
    }
    return res;
}