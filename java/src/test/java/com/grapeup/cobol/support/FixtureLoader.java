package com.grapeup.cobol.support;

import java.io.IOException;
import java.io.InputStream;
import java.nio.charset.StandardCharsets;

public final class FixtureLoader {

    private FixtureLoader() {
    }

    public static String readUtf8(String resourcePath) {
        try (InputStream stream = FixtureLoader.class.getClassLoader().getResourceAsStream(resourcePath)) {
            if (stream == null) {
                throw new IllegalArgumentException("Fixture not found on classpath: " + resourcePath);
            }
            byte[] bytes = stream.readAllBytes();
            return new String(bytes, StandardCharsets.UTF_8).replace("\r\n", "\n");
        } catch (IOException e) {
            throw new RuntimeException("Failed to read fixture: " + resourcePath, e);
        }
    }
}
