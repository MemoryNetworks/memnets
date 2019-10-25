package memnets.lwjgl.util;

import static org.lwjgl.opengl.GL11.glNormal3f;
import static org.lwjgl.opengl.GL11.glTexCoord2f;

public class GLUtils {
    static public final float PI = (float) Math.PI;

    static public float cosf(float r) {
        return (float) Math.cos(r);
    }

    static public float sinf(float r) {
        return (float) Math.sin(r);
    }

    /**
     * Call glNormal3f after scaling normal to unit length.
     */
    static public void normal3f(float x, float y, float z) {
        final float mag = (float) Math.sqrt(x * x + y * y + z * z);
        if (mag > 0.00001F) {
            x /= mag;
            y /= mag;
            z /= mag;
        }
        glNormal3f(x, y, z);
    }

    static public void normal2f(float x, float y) {
        final float mag = (float) Math.sqrt(x * x + y * y);
        if (mag > 0.00001F) {
            x /= mag;
            y /= mag;
        }
        glNormal3f(x, y, 0.0f);
    }

    /**
     * NOTE: doesn't support multi-threaded behavior if need to modify textureFlag
     */
    static public void TXTR_COORD(float x, float y) {
        if (textureFlag) glTexCoord2f(x, y);
    }

    static public boolean textureFlag = false;
}
