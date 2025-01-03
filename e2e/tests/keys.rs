use e2e::{Keys::*, TmuxController};
use std::io::Result;

fn setup() -> Result<TmuxController> {
    let tmux = TmuxController::new()?;
    let _ = tmux.start_sk(None, &["-q", "'foo bar foo-bar'"]);
    tmux.until(|l| l[0].starts_with(">"))?;
    Ok(tmux)
}

#[test]
fn keys_basic() -> Result<()> {
    let tmux = TmuxController::new()?;
    let _ = tmux.start_sk(Some("seq 1 100000"), &[]);
    tmux.until(|l| l[0].starts_with(">") && l[1].starts_with("  100000"))?;
    tmux.send_keys(&[Str("99")])?;
    tmux.until(|l| l[0] == "> 99")?;
    tmux.until(|l| l.len() > 1 && l[1].starts_with("  8146/100000"))?;
    tmux.until(|l| l.len() > 2 && l[2] == "> 99")?;

    Ok(())
}

// Input navigation keys
//
#[test]
fn keys_arrows() -> Result<()> {
    let tmux = setup()?;
    tmux.send_keys(&[Left, Key('|')])?;
    tmux.until(|l| l[0] == "> foo bar foo-ba|r")?;
    tmux.send_keys(&[Right, Key('|')])?;
    tmux.until(|l| l[0] == "> foo bar foo-ba|r|")?;

    Ok(())
}

#[test]
fn keys_ctrl_arrows() -> Result<()> {
    let tmux = setup()?;
    tmux.send_keys(&[Ctrl(&Left), Key('|')])?;
    tmux.until(|l| l[0] == "> foo bar foo-|bar")?;
    tmux.send_keys(&[Ctrl(&Left), Key('|')])?;
    tmux.until(|l| l[0] == "> foo bar |foo-|bar")?;
    tmux.send_keys(&[Ctrl(&Right), Key('|')])?;
    tmux.until(|l| l[0] == "> foo bar |foo-|bar|")?;

    Ok(())
}

#[test]
fn keys_ctrl_a() -> Result<()> {
    let tmux = setup()?;
    tmux.send_keys(&[Ctrl(&Key('a')), Key('|')])?;
    tmux.until(|l| l[0] == "> |foo bar foo-bar")?;

    Ok(())
}

#[test]
fn keys_ctrl_b() -> Result<()> {
    let tmux = setup()?;
    tmux.send_keys(&[Ctrl(&Key('a')), Key('|')])?;
    tmux.until(|l| l[0] == "> |foo bar foo-bar")?;
    tmux.send_keys(&[Ctrl(&Key('f')), Key('|')])?;
    tmux.until(|l| l[0] == "> |f|oo bar foo-bar")?;

    Ok(())
}

#[test]
fn keys_ctrl_e() -> Result<()> {
    let tmux = setup()?;
    tmux.send_keys(&[Ctrl(&Key('a')), Key('|')])?;
    tmux.until(|l| l[0] == "> |foo bar foo-bar")?;
    tmux.send_keys(&[Ctrl(&Key('e')), Key('|')])?;
    tmux.until(|l| l[0] == "> |foo bar foo-bar|")?;

    Ok(())
}

#[test]
fn keys_ctrl_f() -> Result<()> {
    let tmux = setup()?;
    tmux.send_keys(&[Ctrl(&Key('a')), Key('|')])?;
    tmux.until(|l| l[0] == "> |foo bar foo-bar")?;
    tmux.send_keys(&[Ctrl(&Key('f')), Key('|')])?;
    tmux.until(|l| l[0] == "> |f|oo bar foo-bar")?;

    Ok(())
}

#[test]
fn keys_ctrl_h() -> Result<()> {
    let tmux = setup()?;
    tmux.send_keys(&[Ctrl(&Key('h')), Key('|')])?;
    tmux.until(|l| l[0] == "> foo bar foo-ba|")?;

    Ok(())
}

#[test]
fn keys_alt_b() -> Result<()> {
    let tmux = setup()?;
    tmux.send_keys(&[Alt(&Key('b')), Key('|')])?;
    tmux.until(|l| l[0] == "> foo bar foo-|bar")?;

    Ok(())
}

#[test]
fn keys_alt_f() -> Result<()> {
    let tmux = setup()?;
    tmux.send_keys(&[Ctrl(&Key('a')), Key('|')])?;
    tmux.until(|l| l[0] == "> |foo bar foo-bar")?;
    tmux.send_keys(&[Alt(&Key('f')), Key('|')])?;
    tmux.until(|l| l[0] == "> |foo| bar foo-bar")?;

    Ok(())
}

// Input manipulation keys
//
#[test]
fn keys_bspace() -> Result<()> {
    let tmux = setup()?;
    tmux.send_keys(&[BSpace, Key('|')])?;
    tmux.until(|l| l[0] == "> foo bar foo-ba|")?;

    Ok(())
}
#[test]
fn keys_ctrl_d() -> Result<()> {
    let tmux = setup()?;
    tmux.send_keys(&[Ctrl(&Key('a')), Key('|')])?;
    tmux.until(|l| l[0] == "> |foo bar foo-bar")?;
    tmux.send_keys(&[Ctrl(&Key('d')), Key('|')])?;
    tmux.until(|l| l[0] == "> ||oo bar foo-bar")?;

    Ok(())
}
#[test]
fn keys_ctrl_u() -> Result<()> {
    let tmux = setup()?;
    tmux.send_keys(&[Ctrl(&Key('u')), Key('|')])?;
    tmux.until(|l| l[0] == "> |")?;

    Ok(())
}
#[test]
fn keys_ctrl_w() -> Result<()> {
    let tmux = setup()?;
    tmux.send_keys(&[Ctrl(&Key('w')), Key('|')])?;
    tmux.until(|l| l[0] == "> foo bar |")?;

    Ok(())
}
#[test]
fn keys_ctrl_y() -> Result<()> {
    let tmux = setup()?;
    tmux.send_keys(&[Alt(&BSpace), Key('|')])?;
    tmux.until(|l| l[0] == "> foo bar foo-|")?;
    tmux.send_keys(&[Ctrl(&Key('y')), Key('|')])?;
    tmux.until(|l| l[0] == "> foo bar foo-|bar|")?;

    Ok(())
}
#[test]
fn keys_alt_d() -> Result<()> {
    let tmux = setup()?;
    tmux.send_keys(&[Ctrl(&Left), Key('|')])?;
    tmux.until(|l| l[0] == "> foo bar foo-|bar")?;
    tmux.send_keys(&[Ctrl(&Left), Key('|')])?;
    tmux.until(|l| l[0] == "> foo bar |foo-|bar")?;
    tmux.send_keys(&[Alt(&Key('d')), Key('|')])?;
    tmux.until(|l| l[0] == "> foo bar ||-|bar")?;

    Ok(())
}
#[test]
fn keys_alt_bspace() -> Result<()> {
    let tmux = setup()?;
    tmux.send_keys(&[Alt(&BSpace), Key('|')])?;
    tmux.until(|l| l[0] == "> foo bar foo-|")?;

    Ok(())
}

// Results navigation keys
//
#[test]
fn keys_ctrl_k() -> Result<()> {
    let tmux = TmuxController::new()?;
    let _ = tmux.start_sk(Some("seq 1 100000"), &[]);
    tmux.until(|l| l[0].starts_with(">") && l[1].starts_with("  100000"))?;
    tmux.send_keys(&[Ctrl(&Key('k'))])?;
    tmux.until(|l| l.len() > 2 && l[2] == "  1")?;
    tmux.until(|l| l.len() > 3 && l[3] == "> 2")?;

    Ok(())
}

#[test]
fn keys_tab() -> Result<()> {
    let tmux = TmuxController::new()?;
    let _ = tmux.start_sk(Some("seq 1 100000"), &[]);
    tmux.until(|l| l[0].starts_with(">") && l[1].starts_with("  100000"))?;
    tmux.send_keys(&[Ctrl(&Key('k'))])?;
    tmux.until(|l| l.len() > 2 && l[2] == "  1")?;
    tmux.until(|l| l.len() > 3 && l[3] == "> 2")?;

    tmux.send_keys(&[Tab])?;
    tmux.until(|l| l.len() > 2 && l[2] == "> 1")?;
    tmux.until(|l| l.len() > 3 && l[3] == "  2")?;

    Ok(())
}

#[test]
fn keys_btab() -> Result<()> {
    let tmux = TmuxController::new()?;
    let _ = tmux.start_sk(Some("seq 1 100000"), &[]);
    tmux.until(|l| l[0].starts_with(">") && l[1].starts_with("  100000"))?;
    tmux.send_keys(&[BTab])?;
    tmux.until(|l| l.len() > 2 && l[2] == "  1")?;
    tmux.until(|l| l.len() > 3 && l[3] == "> 2")?;

    Ok(())
}

#[test]
fn keys_enter() -> Result<()> {
    let tmux = TmuxController::new()?;
    let outfile = tmux.start_sk(Some("seq 1 100000"), &[])?;
    tmux.until(|l| l[0].starts_with(">") && l[1].starts_with("  100000"))?;
    tmux.send_keys(&[Enter])?;
    tmux.until(|l| !l[0].starts_with(">"))?;
    let res = tmux.output(&outfile)?;
    assert_eq!(res[0], "1");
    Ok(())
}
#[test]
fn keys_ctrl_m() -> Result<()> {
    let tmux = TmuxController::new()?;
    let outfile = tmux.start_sk(Some("seq 1 100000"), &[])?;
    tmux.until(|l| l[0].starts_with(">") && l[1].starts_with("  100000"))?;
    tmux.send_keys(&[Ctrl(&Key('m'))])?;
    tmux.until(|l| !l[0].starts_with(">"))?;
    let res = tmux.output(&outfile)?;
    assert_eq!(res[0], "1");
    Ok(())
}
