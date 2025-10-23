package com.grapeup.cobol.support;

import java.util.*;

public class CobolContext {
    private final Map<String, Object> variables;

    public CobolContext() {
        this.variables = new HashMap<>();
    }

    public CobolContext(Map<String, Object> vars) {
        this.variables = new HashMap<>(vars);
    }

    public CobolContext with(String name, Object value) {
        variables.put(name.toUpperCase(), value);
        return this;
    }

    public Object get(String name) {
        return variables.get(name.toUpperCase());
    }

    public int getInt(String name) {
        return ((Number) get(name)).intValue();
    }

    public Map<String, Object> asMap() {
        return Collections.unmodifiableMap(variables);
    }
}
