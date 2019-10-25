package memnets.lwjgl.util;

import static memnets.lwjgl.util.GLUtils.*;
import static org.lwjgl.opengl.GL11.*;

/**
 * draws a unit normal cylinder oriented along the z axis.
 * The base of the cylinder is placed at z = 0, and the top at z = 1.0
 * slices the number of subdivisions around the z axis.
 * <p>
 * <p>
 * If texturing is turned on (GLUtils.textureFlag), then texture
 * coordinates are generated so that t ranges linearly from 0.0 at z = 0 to
 * 1.0 at z = height, and s ranges from 0.0 at the +y axis, to 0.25 at the +x
 * axis, to 0.5 at the -y axis, to 0.75 at the -x axis, and back to 1.0 at the
 * +y axis.
 */
public class Cylinder {
    private final int slices;
    private final float[] xpoints;
    private final float[] ypoints;

    public Cylinder(int slices) {
        this.slices = slices;
        float da = 2.0f * PI / slices;
        xpoints = new float[slices + 1];
        ypoints = new float[slices + 1];
        for (int i = 0; i <= slices; i++) {
            if (i == slices) {
                xpoints[i] = sinf(0.0f);
                ypoints[i] = cosf(0.0f);
            } else {
                xpoints[i] = sinf(i * da);
                ypoints[i] = cosf(i * da);
            }
        }
    }

    public void draw() {
        float ds = 1.0f / slices;
        float h = 1.0f;

        float x, y;
        int i;

        glBegin(GL_TRIANGLE_FAN);
        normal3f(0.0f, 0.0f, -1.0f);
        glVertex3f(0.0f, 0.0f, 0.0f);
        for (i = 0; i < xpoints.length; i++) {
            glVertex3f(xpoints[i], ypoints[i], 0.0f);
        }
        glEnd();

        float s = 0.0f;
        glBegin(GL_QUAD_STRIP);
        for (i = 0; i < xpoints.length; i++) {
            x = xpoints[i];
            y = ypoints[i];
            normal3f(x, y, 0.0f);
            TXTR_COORD(s, 0.0f);
            glVertex3f(x, y, 0.0f);
            normal3f(x, y, 0.0f);
            TXTR_COORD(s, h);
            glVertex3f(x, y, h);
            s += ds;
        } // for slices
        glEnd();

        glRotatef(180.0f, 1.0f, 0.0f, 0.0f);
        glBegin(GL_TRIANGLE_FAN);
        glVertex3f(0.0f, 0.0f, -h);
        for (i = 0; i < xpoints.length; i++) {
            glVertex3f(xpoints[i], ypoints[i], -h);
        }
        glEnd();
    }
}
