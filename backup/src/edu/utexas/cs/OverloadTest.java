package edu.utexas.cs;
import java.util.Iterator;
import java.util.List;
import java.util.Map;

import soot.PackManager;
import soot.Scene;
import soot.SceneTransformer;
import soot.SootMethod;
import soot.Transform;
import soot.Body;
import soot.BodyTransformer;
import soot.G;
import soot.Local;
import soot.PackManager;
import soot.Transform;
import soot.Unit;
import soot.jimple.DefinitionStmt;
import soot.jimple.GotoStmt;
import soot.jimple.IfStmt;
import soot.jimple.InstanceFieldRef;
import soot.jimple.IntConstant;
import soot.jimple.ReturnStmt;
import soot.toolkits.graph.ExceptionalUnitGraph;
import soot.util.cfgcmd.CFGToDotGraph;

import com.microsoft.z3.BoolExpr;
import com.microsoft.z3.Expr;
import com.microsoft.z3.IntExpr;
import com.microsoft.z3.Solver;
import com.microsoft.z3.Status;
import com.microsoft.z3.Z3Exception;

public class OverloadTest {

    public static void main(String[] args) {
        PackManager.v().getPack("jtp").add(
                                           new Transform("jtp.OverloadTransform", new SceneTransformer() {

                                                   protected void internalTransform(String phasename, Map options) {
                                                       Scene scene = Scene.v();
                                                       if (scene.containsMethod("test01.compare"))
                                                           {
                                                               SootMethod method = scene.getMethod("test01.compare");
                                                               if (method.hasActiveBody()) {
                                                                   Body body = method.getActiveBody();
                                                               
                                                                   ExceptionalUnitGraph eug = new ExceptionalUnitGraph(body);
                                                                   ConsistencyAnalysis b = new ConsistencyAnalysis(eug);
                                                                   List<Unit> heads = eug.getHeads();
                                                                   try {
                                                                       for (Iterator<Unit> i1 = heads.iterator(); i1.hasNext(); ) {
                                                                           Unit u1 = i1.next();
                                                                           AnalysisContainer conds = b.getFlowAfter(u1);
                                                                           G.v().out.println("The flow at this head is " + conds.toString());

                                                                           String p0 = body.getParameterLocal(0).toString();
                                                                           String p1 = body.getParameterLocal(1).toString();
                                                                           IntExpr e0=b.m1.get(p0);
                                                                           IntExpr e1=b.m1.get(p1);
                                                                           IntExpr e[] = new IntExpr[3]; String o[] = new String[3];
                                                                           for (int i2 = 0; i2 < 3; ++i2) {
                                                                               o[i2] = i2 < 1 ? "o": (o[i2 - 1] + "_"); while (b.m1.containsKey(o[i2])) o[i2] += "_";
                                                                               b.m1.put(o[i2], e[i2] = b.ctx.MkIntConst(o[i2]));
                                                                           }
                                                                           BoolExpr reflexCond = (BoolExpr) conds.eq0.Substitute(new Expr[] {e0, e1}, new Expr[] {e[0], e[0]});
                                                                           BoolExpr symmCond = b.ctx.MkIff(
                                                                                                           (BoolExpr) conds.lt0.Substitute(new Expr[] {e0, e1}, new Expr[] {e[0], e[1]}),
                                                                                                           (BoolExpr) conds.gt0.Substitute(new Expr[] {e0, e1}, new Expr[] {e[1], e[0]}));
                                                                           BoolExpr transCond = b.ctx.MkImplies(b.ctx.MkAnd(new BoolExpr[]
                                                                               {(BoolExpr) conds.lt0.Substitute(new Expr[] {e0, e1}, new Expr[] {e[0], e[1]}),
                                                                                (BoolExpr) conds.lt0.Substitute(new Expr[] {e0, e1}, new Expr[] {e[1], e[2]})}),
                                                                                                                (BoolExpr) conds.lt0.Substitute(new Expr[] {e0, e1}, new Expr[] {e[0], e[2]}));
                                                                           Solver solver = b.ctx.MkSolver();
                                                                           solver.Push(); solver.Assert(b.ctx.MkNot(reflexCond));
                                                                           G.v().out.println("reflexCond is " + (solver.Check() == Status.UNSATISFIABLE ? "proved" : "not proved"));
                                                                           solver.Pop();
                                                                           solver.Push(); solver.Assert(b.ctx.MkNot(symmCond));
                                                                           G.v().out.println("symmCond is " + (solver.Check() == Status.UNSATISFIABLE ? "proved" : "not proved"));
                                                                           solver.Pop();
                                                                           solver.Push(); solver.Assert(b.ctx.MkNot(transCond));
                                                                           G.v().out.println("transCond is " + (solver.Check() == Status.UNSATISFIABLE ? "proved" : "not proved"));
                                                                           solver.Pop();
                                                               
                                                                       }
                                                                   } catch (Z3Exception E) {
                                                                       E.printStackTrace();
                                                                   }    
                                                    
                                                               }
                                                           } else {
                                                           SootMethod method = scene.getMethod("test01.compare");
                                                           G.v().out.println("method " + (method == null ? "is" : "is not") + " null.");
                                                       }
                                                   }

                                               }));
		
        soot.Main.main(args);
    }

}
