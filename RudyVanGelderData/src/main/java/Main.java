import java.io.*;
import java.net.HttpURLConnection;
import java.net.URL;
import java.util.*;

import org.json.*;
import org.w3c.dom.ls.LSOutput;

public class Main {

    public static JSONArray result;
    static List<String> artists = new ArrayList<>();

    public static void main(String[] args) {
        StringBuilder sb = new StringBuilder();
        JSONArray finalArray = new JSONArray();

        File cache = new File("cache.txt");
        if (!cache.exists()) {
            for (int i = 1; i < 12; i++) {
                System.out.println("Fetching data... (" + i + "/11)");
                JSONArray temp = (new JSONObject(fetch("https://api.discogs.com/artists/252966/releases?per_page=500&page=" + i))).getJSONArray("releases");
                for (Object o : temp) {
                    finalArray.put(o);
                }
            }
            result = finalArray;
        } else {
            try {
                BufferedReader br = new BufferedReader(new FileReader(cache));
                StringBuilder sb2 = new StringBuilder();
                String line = br.readLine();

                while (line != null) {
                    sb2.append(line);
                    sb2.append(System.lineSeparator());
                    line = br.readLine();
                }
                String everything = sb2.toString();
                result = new JSONArray(everything);
            } catch (IOException e) {
                throw new RuntimeException(e);
            }
        }
        try {
            FileWriter fw = new FileWriter("cache.txt");
            fw.write(result.toString());
            fw.close();
        } catch (IOException e) {
            throw new RuntimeException(e);
        }
        generateNodes();
        generateEdges();
        System.out.println("Succesfully generated Edges and Nodes!");

    }

    private static String getReturnValue(HttpURLConnection con) throws IOException {
        InputStream in = con.getInputStream();
        BufferedReader reader = new BufferedReader(new InputStreamReader(in));
        String s;
        StringBuilder sb = new StringBuilder();
        while ((s = reader.readLine()) != null) {
            sb.append(s);
        }
        return sb.toString();
    }

    public static String fetch(String url) {
        try {
            URL urlObj = new URL(url);
            HttpURLConnection con = (HttpURLConnection) urlObj.openConnection();
            con.setRequestMethod("GET");
            con.setRequestProperty("Accept", "application/json");
            con.setRequestProperty("Content-Type", "application/json");
            con.setRequestProperty("Authorizaion", "Bearer IsalIyVzuBRJgMFikzdzssojQhWOHpHOZYNWlSLG");
            con.setDoOutput(true);
            return getReturnValue(con);
        } catch (IOException e) {
            throw new RuntimeException(e);
        }
    }

    public static void generateNodes() {
        try {
            FileWriter fw = new FileWriter("nodesRVG.csv");
            fw.write("\"artist\",\"artist_id\"\n");

            int id = 1;
            for (int i = 0; i < result.length(); i++) {
                String temp = result.getJSONObject(i).getString("artist");
                artists = checkArtist(artists, temp);
            }
            for (String artist : artists) {
                fw.write("\"" + artist + "\",");
                fw.write(String.valueOf(id++));
                fw.write("\n");
            }
            fw.close();
        } catch (IOException e) {
            throw new RuntimeException(e);
        }
    }

    private static List<String> checkArtist(List<String> list, String artist) {
        boolean skip = false;
        if (list.contains(artist)) {
            skip = true;
            return list;
        }
        if (artist.contains(" / ")) {
            for (String s : artist.split(" / ")) {
                checkArtist(list, s);
            }
            skip = true;
        }
        if (artist.contains(", ") && !artist.contains(", Jr.")) {
            for (String s : artist.split(", (?!Jr\\.)")) {
                checkArtist(list, s);
            }
            skip = true;
        }
        if (artist.contains(" - ")) {
            for (String s : artist.split(" - ")) {
                checkArtist(list, s);
            }
            skip = true;
        }
        if (artist.contains(" & ")) {
            for (String s : artist.split(" & ")) {
                checkArtist(list, s);
            }
            skip = true;
        }
        if (artist.contains(" · ")) {
            for (String s : artist.split(" · ")) {
                checkArtist(list, s);
            }
            skip = true;
        }
        if (artist.contains(" Featuring ")) {
            for (String s : artist.split(" Featuring ")) {
                checkArtist(list, s);
            }
            skip = true;
        }
        if (artist.contains(" : ")) {
            for (String s : artist.split(" : ")) {
                checkArtist(list, s);
            }
            skip = true;
        }
        if (artist.contains(" ● ")) {
            for (String s : artist.split(" ● ")) {
                checkArtist(list, s);
            }
            skip = true;
        }
        if (artist.contains(" with ")) {
            for (String s : artist.split(" with ")) {
                checkArtist(list, s);
            }
            skip = true;
        }
        if (artist.contains(" With ")) {
            for (String s : artist.split(" With ")) {
                checkArtist(list, s);
            }
            skip = true;
        }
        if (!skip) {
            list.add(artist);
        }
        return list;
    }


    public static void generateEdges() {
        try {
            FileWriter fw = new FileWriter("edgesRVG.csv");
            fw.write("\"source\",\"target\",\"year\",\"title\",\"type\"\n");
            List<List<String>> edges = new ArrayList<>();
            for (int h = 0; h < result.length(); h++) {
                String temp = result.getJSONObject(h).getString("artist");
                if (!artists.contains(temp)) {
                    List<String> singular = new ArrayList<>();
                    checkArtist(singular, temp);
                    for (int i = 0; i < singular.size(); i++) {
                        for (int j = i + 1; j < singular.size(); j++) {
                            String year = null;
                            if (!result.getJSONObject(h).isNull("year")) {
                                year = String.valueOf(result.getJSONObject(h).getInt("year"));
                            }
                            List<String> pair = Arrays.asList(singular.get(i), singular.get(j), year, result.getJSONObject(h).getString("title"), result.getJSONObject(h).getString("type"));
                            edges.add(pair);
                        }
                    }

                    for (String interpreter : singular) {
                        addRudy(edges, h, interpreter);
                    }
                } else {
                    addRudy(edges, h, temp);
                }
            }
            for (List<String> edge : edges) {
                fw.write("\"" + edge.get(0).replaceAll(",", "|") + "\", \"" + edge.get(1).replaceAll(",", "|") + "\",\"" + edge.get(2) + "\", \"" + edge.get(3).replaceAll(",", "|") + "\",\"" + edge.get(4).replaceAll(",", "|") + "\"\n");
            }
            fw.close();
        } catch (IOException e) {
            throw new RuntimeException(e);
        }
    }

    private static void addRudy(List<List<String>> edges, int h, String temp) {
        String year = null;
        if (!result.getJSONObject(h).isNull("year")) {
            year = String.valueOf(result.getJSONObject(h).getInt("year"));
        }
        edges.add(Arrays.asList(temp, "Rudy Van Gelder", year, result.getJSONObject(h).getString("title"), result.getJSONObject(h).getString("type")));
    }
}
