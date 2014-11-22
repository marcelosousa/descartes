import com.microsoft.z3.BoolExpr;


public class AnalysisContainer {
    BoolExpr lt0;
    BoolExpr eq0;
    BoolExpr gt0;

    @Override
    public String toString() {
        return "{" + this.lt0.toString() + ", " + this.eq0.toString() + ", " + this.gt0.toString() + "}";
    }

}