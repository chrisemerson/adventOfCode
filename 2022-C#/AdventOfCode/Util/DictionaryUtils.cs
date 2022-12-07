namespace AdventOfCode;

public static class DictionaryUtils
{
    public static Dictionary<TKey, TValue> Replace<TKey, TValue> (
        this Dictionary<TKey, TValue> dictionary,
        TKey key,
        TValue newValue
    ) where TKey : notnull {
        if (dictionary.ContainsKey(key)) {
            dictionary.Remove(key);
        }

        dictionary.Add(key, newValue);

        return dictionary;
    }
}
