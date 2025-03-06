package main

import (
	"context"
	"flag"
	"fmt"
	"net/http"
	"os"
	"path/filepath"
	"regexp"
	"strings"

	"github.com/google/go-github/v57/github"
	"github.com/olekukonko/tablewriter"
	"golang.org/x/oauth2"
)

const (
	defaultWorkflowDir = ".github/workflows"
)

// GitHubActionsUpdater は GitHub Actions 更新ツールの構造体です
type GitHubActionsUpdater struct {
	dryRun        bool
	verbose       bool
	workflowFiles []string
	client        *github.Client
	latestVersions map[string]string
}

// NewGitHubActionsUpdater は新しい updater インスタンスを作成します
func NewGitHubActionsUpdater(options Options) *GitHubActionsUpdater {
	updater := &GitHubActionsUpdater{
		dryRun:        options.DryRun,
		verbose:       options.Verbose,
		workflowFiles: options.WorkflowFiles,
		latestVersions: make(map[string]string),
	}

	// GitHub クライアントの初期化
	var tc *http.Client
	token := options.Token
	if token == "" {
		token = os.Getenv("GITHUB_TOKEN")
	}

	if token != "" {
		ctx := context.Background()
		ts := oauth2.StaticTokenSource(
			&oauth2.Token{AccessToken: token},
		)
		tc = oauth2.NewClient(ctx, ts)
	}

	updater.client = github.NewClient(tc)

	// ワークフローファイルが指定されていない場合は、デフォルトディレクトリから全て取得
	if len(updater.workflowFiles) == 0 {
		updater.workflowFiles = updater.findAllWorkflowFiles()
	}

	if updater.verbose && token == "" {
		fmt.Println("警告: GitHub トークンが提供されていません。API リクエストがレート制限される可能性があります。")
	}

	return updater
}

// Options はツールのオプション設定を保持する構造体です
type Options struct {
	WorkflowFiles []string
	DryRun        bool
	Verbose       bool
	Token         string
	ListFiles     bool
}

// Run は更新処理を実行します
func (u *GitHubActionsUpdater) Run() error {
	if len(u.workflowFiles) == 0 {
		return fmt.Errorf("%s ディレクトリにワークフローファイルが見つかりませんでした", defaultWorkflowDir)
	}

	// 指定されたファイルが存在するか検証
	if err := u.validateWorkflowFiles(); err != nil {
		return err
	}

	fmt.Printf("%d 個のワークフローファイルの GitHub Actions をチェックしています...\n", len(u.workflowFiles))

	// ハッシュベースのアクションを検索
	hashBasedActions, err := u.findHashBasedActions()
	if err != nil {
		return err
	}

	if len(hashBasedActions) == 0 {
		fmt.Println("指定されたワークフローファイルにハッシュベースの GitHub Actions は見つかりませんでした。")
		return nil
	}

	fmt.Printf("%d 個のハッシュベースの GitHub Actions が見つかりました:\n", len(hashBasedActions))
	for action, versions := range hashBasedActions {
		plural := "バージョン"
		if len(versions) > 1 {
			plural = "バージョン"
		}
		fmt.Printf("  - %s (%d 個のハッシュベース%s)\n", action, len(versions), plural)
	}

	// 識別された全てのアクションの最新バージョンを取得
	fmt.Println("\nこれらのアクションの最新バージョンを取得しています...")
	
	for action := range hashBasedActions {
		latestVersion, err := u.fetchLatestVersion(action)
		if err != nil {
			fmt.Printf("警告: %s の最新バージョンを取得できませんでした: %v\n", action, err)
			continue
		}
		u.latestVersions[action] = latestVersion
		fmt.Printf("%s の最新バージョン: %s\n", action, latestVersion)
	}

	// ワークフローファイルを更新
	return u.updateWorkflowFiles(hashBasedActions)
}

// findAllWorkflowFiles は defaultWorkflowDir 内の全ての YAML ファイルを検索します
func (u *GitHubActionsUpdater) findAllWorkflowFiles() []string {
	workflowDir := filepath.Join(".", defaultWorkflowDir)
	
	// ディレクトリが存在するか確認
	if _, err := os.Stat(workflowDir); os.IsNotExist(err) {
		if u.verbose {
			fmt.Printf("ワークフローディレクトリが %s に見つかりませんでした\n", workflowDir)
		}
		return []string{}
	}

	var files []string
	filepath.Walk(workflowDir, func(path string, info os.FileInfo, err error) error {
		if err != nil {
			return err
		}
		if !info.IsDir() {
			ext := strings.ToLower(filepath.Ext(path))
			if ext == ".yml" || ext == ".yaml" {
				files = append(files, path)
			}
		}
		return nil
	})

	return files
}

// validateWorkflowFiles は全ての指定されたファイルが存在することを確認します
func (u *GitHubActionsUpdater) validateWorkflowFiles() error {
	var invalidFiles []string

	for _, file := range u.workflowFiles {
		if _, err := os.Stat(file); os.IsNotExist(err) {
			invalidFiles = append(invalidFiles, file)
		}
	}

	if len(invalidFiles) > 0 {
		fmt.Println("エラー: 以下のワークフローファイルが存在しません:")
		for _, file := range invalidFiles {
			fmt.Printf("  - %s\n", file)
		}
		return fmt.Errorf("無効なワークフローファイルが指定されました")
	}

	return nil
}

// findHashBasedActions はワークフローファイルからハッシュベースのアクション参照を検索します
func (u *GitHubActionsUpdater) findHashBasedActions() (map[string][]string, error) {
	hashBasedActions := make(map[string][]string)
	shaPattern := regexp.MustCompile(`^[0-9a-f]{40}$`)

	for _, file := range u.workflowFiles {
		content, err := os.ReadFile(file)
		if err != nil {
			if u.verbose {
				fmt.Printf("%s の処理中にエラーが発生しました: %v\n", file, err)
			}
			continue
		}

		// 'uses: action@hash' パターンを検索
		actionPattern := regexp.MustCompile(`uses:\s+([^@\s]+)@([^\s#]+)(?:\s+#\s+(.+))?`)
		matches := actionPattern.FindAllStringSubmatch(string(content), -1)

		for _, match := range matches {
			if len(match) >= 3 {
				action := match[1]
				version := match[2]
				comment := ""
				if len(match) > 3 {
					comment = match[3]
				}

				// SHA ハッシュ（40桁の16進数）かチェック
				if shaPattern.MatchString(version) {
					versionStr := version
					if comment != "" {
						versionStr = version + " # " + comment
					}
					
					// 既存のバージョンリストを取得
					versions, exists := hashBasedActions[action]
					if !exists {
						versions = []string{}
					}
					
					// 重複チェック
					found := false
					for _, v := range versions {
						if v == versionStr {
							found = true
							break
						}
					}
					
					if !found {
						versions = append(versions, versionStr)
						hashBasedActions[action] = versions
					}
				}
			}
		}
	}

	return hashBasedActions, nil
}

// fetchLatestVersion は指定されたアクションリポジトリの最新バージョンを取得します
func (u *GitHubActionsUpdater) fetchLatestVersion(actionRepo string) (string, error) {
	// キャッシュをチェック
	if v, ok := u.latestVersions[actionRepo]; ok {
		return v, nil
	}

	if u.verbose {
		fmt.Printf("%s の最新バージョンを取得中...\n", actionRepo)
	}

	ctx := context.Background()

	// owner/repo 形式からパーツを抽出
	parts := strings.Split(actionRepo, "/")
	if len(parts) < 2 {
		return "", fmt.Errorf("無効なリポジトリ形式: %s", actionRepo)
	}
	
	owner := parts[0]
	repo := strings.Join(parts[1:], "/")

	// 最新リリースを取得
	release, _, err := u.client.Repositories.GetLatestRelease(ctx, owner, repo)
	if err != nil {
		return "", fmt.Errorf("GitHub API エラー: %v", err)
	}

	latestTag := release.GetTagName()

	// タグの参照を取得して SHA を見つける
	ref, _, err := u.client.Git.GetRef(ctx, owner, repo, "tags/"+latestTag)
	if err != nil {
		// タグだけを返す
		return latestTag, nil
	}

	latestSHA := ref.Object.GetSHA()
	if latestSHA != "" {
		return latestSHA + " # " + latestTag, nil
	}

	// SHA が取得できない場合はタグだけを返す
	return latestTag, nil
}

// updateWorkflowFiles はワークフローファイルを更新します
func (u *GitHubActionsUpdater) updateWorkflowFiles(hashBasedActions map[string][]string) error {
	filesChanged := 0
	actionsUpdated := make(map[string]int)

	for _, file := range u.workflowFiles {
		content, err := os.ReadFile(file)
		if err != nil {
			if u.verbose {
				fmt.Printf("%s の読み取り中にエラーが発生しました: %v\n", file, err)
			}
			continue
		}

		contentStr := string(content)
		fileModified := false

		for action, versions := range hashBasedActions {
			for _, oldVersion := range versions {
				// このアクションの最新バージョンがない場合はスキップ
				latestVersion, ok := u.latestVersions[action]
				if !ok {
					continue
				}

				// 'uses: action@oldversion' パターンをマッチ
				oldPattern := fmt.Sprintf("uses: %s@%s", action, oldVersion)
				newPattern := fmt.Sprintf("uses: %s@%s", action, latestVersion)

				// 古いバージョンを新しいバージョンに置き換え
				if strings.Contains(contentStr, oldPattern) {
					contentStr = strings.ReplaceAll(contentStr, oldPattern, newPattern)
					actionsUpdated[action]++
					fileModified = true
				}
			}
		}

		if fileModified {
			if u.dryRun {
				fmt.Printf("更新予定: %s\n", file)
			} else {
				err := os.WriteFile(file, []byte(contentStr), 0644)
				if err != nil {
					return fmt.Errorf("%s の書き込み中にエラーが発生しました: %v", file, err)
				}
				fmt.Printf("更新しました: %s\n", file)
				filesChanged++
			}
		} else {
			fmt.Printf("更新の必要はありません: %s\n", file)
		}
	}

	if filesChanged > 0 {
		fmt.Printf("\n%d 個のワークフローファイルを正常に更新しました\n", filesChanged)

		// 表形式で更新されたアクションを表示
		table := tablewriter.NewWriter(os.Stdout)
		table.SetHeader([]string{"アクション", "更新回数"})
		table.SetCaption(true, "更新されたアクション")

		for action, count := range actionsUpdated {
			table.Append([]string{action, fmt.Sprintf("%d", count)})
		}
		table.Render()
	} else {
		dryRunMsg := ""
		if u.dryRun {
			dryRunMsg = "（ドライラン）"
		}
		fmt.Printf("\nファイルは更新されませんでした %s\n", dryRunMsg)
	}

	return nil
}

// ListWorkflowFiles は利用可能なワークフローファイルを一覧表示します
func (u *GitHubActionsUpdater) ListWorkflowFiles() []string {
	workflowDir := filepath.Join(".", defaultWorkflowDir)
	
	// ディレクトリが存在するか確認
	if _, err := os.Stat(workflowDir); os.IsNotExist(err) {
		fmt.Printf("ワークフローディレクトリが %s に見つかりませんでした\n", workflowDir)
		return []string{}
	}

	var files []string
	filepath.Walk(workflowDir, func(path string, info os.FileInfo, err error) error {
		if err != nil {
			return err
		}
		if !info.IsDir() {
			ext := strings.ToLower(filepath.Ext(path))
			if ext == ".yml" || ext == ".yaml" {
				files = append(files, path)
			}
		}
		return nil
	})

	// 表形式で利用可能なワークフローファイルを表示
	table := tablewriter.NewWriter(os.Stdout)
	table.SetHeader([]string{"#", "パス"})
	table.SetCaption(true, "利用可能なワークフローファイル")

	for i, file := range files {
		relativePath, err := filepath.Rel(".", file)
		if err != nil {
			relativePath = file
		}
		table.Append([]string{fmt.Sprintf("%d", i+1), relativePath})
	}
	table.Render()

	return files
}

func main() {
	// コマンドラインオプションの設定
	var options Options

	// ワークフローファイルオプションを複数回指定できるようにするためのカスタム変数
	var fileFlags fileFlagValue
	flag.Var(&fileFlags, "file", "更新するワークフローファイルを指定（複数回使用可能、相対パスまたは絶対パス）。短縮形: -f")
	flag.Var(&fileFlags, "f", "更新するワークフローファイルを指定（複数回使用可能、相対パスまたは絶対パス）")

	// その他のオプション
	flag.BoolVar(&options.ListFiles, "list", false, "利用可能なワークフローファイルを一覧表示。短縮形: -l")
	flag.BoolVar(&options.ListFiles, "l", false, "利用可能なワークフローファイルを一覧表示")
	flag.BoolVar(&options.DryRun, "dry-run", false, "変更を行わずに何が行われるかを表示。短縮形: -n")
	flag.BoolVar(&options.DryRun, "n", false, "変更を行わずに何が行われるかを表示")
	flag.BoolVar(&options.Verbose, "verbose", false, "より詳細な出力を表示。短縮形: -v")
	flag.BoolVar(&options.Verbose, "v", false, "より詳細な出力を表示")
	flag.StringVar(&options.Token, "token", "", "レート制限を回避するための GitHub API トークン。短縮形: -t")
	flag.StringVar(&options.Token, "t", "", "レート制限を回避するための GitHub API トークン")

	// ヘルプメッセージ
	flag.Usage = func() {
		fmt.Fprintf(os.Stderr, "使用法: %s [オプション] [-f ワークフローファイル ...]\n\n", os.Args[0])
		flag.PrintDefaults()
	}

	flag.Parse()

	// ファイルオプションを設定
	options.WorkflowFiles = fileFlags.Values

	// GitHubActionsUpdater インスタンスの作成
	updater := NewGitHubActionsUpdater(options)

	// コマンドの実行
	if options.ListFiles {
		updater.ListWorkflowFiles()
	} else {
		err := updater.Run()
		if err != nil {
			fmt.Fprintf(os.Stderr, "エラー: %v\n", err)
			os.Exit(1)
		}
	}
}

// fileFlagValue は複数回指定できるフラグのための型です
type fileFlagValue struct {
	Values []string
}

func (f *fileFlagValue) String() string {
	return strings.Join(f.Values, ", ")
}

func (f *fileFlagValue) Set(value string) error {
	// 相対パスを絶対パスに変換
	absPath, err := filepath.Abs(value)
	if err != nil {
		return err
	}
	f.Values = append(f.Values, absPath)
	return nil
}