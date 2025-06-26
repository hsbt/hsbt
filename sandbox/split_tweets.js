/*
 * Twitter Archive 分割・振り分けスクリプト
 * 
 * 機能:
 * 1. ツイートデータの年月別分割
 * 2. メディアファイルの年月別分割（ツイートデータ連携方式）
 * 
 * 使用方法:
 * node split_tweets.js [出力ディレクトリパス] [入力ディレクトリパス]
 * 
 * 例:
 * node split_tweets.js                                    # カレントディレクトリのdataフォルダから読み込み、カレントディレクトリに出力
 * node split_tweets.js /output/path                       # カレントディレクトリのdataフォルダから読み込み、指定パスに出力
 * node split_tweets.js /output/path /input/archive/path   # 指定パスのdataフォルダから読み込み、指定パスに出力
 * 
 * 処理順序:
 * 1. ツイートデータを年月別に分割してJSONファイルを作成
 * 2. メディアファイルをツイートデータと連携して年月別に分割
 */

const fs = require('fs');
const path = require('path');

// 基準ディレクトリを取得（引数またはカレントディレクトリ）
function getDirectories() {
  const args = process.argv.slice(2);
  
  let outputDir, inputDir;
  
  if (args.length === 0) {
    // 引数なし: カレントディレクトリのdataから読み込み、カレントディレクトリに出力
    outputDir = process.cwd();
    inputDir = process.cwd();
  } else if (args.length === 1) {
    // 引数1つ: カレントディレクトリのdataから読み込み、指定パスに出力
    outputDir = path.resolve(args[0]);
    inputDir = process.cwd();
  } else {
    // 引数2つ: 指定パスのdataから読み込み、指定パスに出力
    outputDir = path.resolve(args[0]);
    inputDir = path.resolve(args[1]);
  }
  
  // 入力ディレクトリの検証
  if (!fs.existsSync(inputDir)) {
    console.error(`エラー: 入力ディレクトリが存在しません: ${inputDir}`);
    process.exit(1);
  }
  const inputDataDir = path.join(inputDir, 'data');
  if (!fs.existsSync(inputDataDir)) {
    console.error(`エラー: 入力ディレクトリにdataフォルダが存在しません: ${inputDataDir}`);
    process.exit(1);
  }
  
  // 出力ディレクトリの作成
  if (!fs.existsSync(outputDir)) {
    try {
      fs.mkdirSync(outputDir, { recursive: true });
      console.log(`出力ディレクトリを作成しました: ${outputDir}`);
    } catch (error) {
      console.error(`エラー: 出力ディレクトリの作成に失敗しました: ${outputDir}`, error);
      process.exit(1);
    }
  }
  
  console.log(`入力ディレクトリ: ${inputDir}`);
  console.log(`出力ディレクトリ: ${outputDir}`);
  return { inputDir, outputDir };
}

// データディレクトリを作成
function ensureDirectoryExists(dirPath) {
  if (!fs.existsSync(dirPath)) {
    fs.mkdirSync(dirPath, { recursive: true });
  }
}

// created_atから年月を取得
function getYearMonth(createdAt) {
  const date = new Date(createdAt);
  const year = date.getFullYear();
  const month = String(date.getMonth() + 1).padStart(2, '0');
  return { year, month };
}

// Twitter Snowflake IDから日時を取得
function getDateFromSnowflakeId(snowflakeId) {
  // Twitter Snowflake ID format: 42 bits for timestamp + 10 bits for machine ID + 12 bits for sequence
  // Twitter epoch: 1288834974657 (2010-11-04T01:42:54.657Z)
  const TWITTER_EPOCH = 1288834974657;
  
  // Snowflake IDから最初の42ビット（タイムスタンプ）を取得
  const timestamp = parseInt(snowflakeId) >> 22;
  const twitterTimestamp = timestamp + TWITTER_EPOCH;
  
  return new Date(twitterTimestamp);
}

// ファイル名からTwitter Snowflake IDを抽出
function extractSnowflakeFromFilename(filename) {
  // ファイル名形式: {tweet_id}-{media_id}.{extension}
  const match = filename.match(/^(\d+)-/);
  return match ? match[1] : null;
}

// メディアファイル名からメディアIDを抽出（distribute_media.jsから）
function extractMediaId(filename) {
  // ファイル名の形式: 229431811527278592-Ay8TXz8CEAAQykT.jpg
  // メディアIDは - 以降の拡張子を除いた部分
  const match = filename.match(/^\d+-(.+)\.\w+$/);
  return match ? match[1] : null;
}

// 指定した年月のJSONファイルを読み込み（distribute_media.jsから）
function loadTweetsForMonth(year, month, inputDir) {
  const filepath = path.join(inputDir, 'data', year, `${month}.json`);
  
  if (!fs.existsSync(filepath)) {
    return [];
  }
  
  try {
    const content = fs.readFileSync(filepath, 'utf8');
    return JSON.parse(content);
  } catch (error) {
    console.error(`エラー: ${filepath} の読み込みに失敗:`, error);
    return [];
  }
}

// tweets.jsファイルを読み込んでパース
function loadTweets(inputDir) {
  console.log('tweets.jsとtweets-part1.jsファイルを読み込み中...');
  
  let allTweets = [];
  
  // tweets.js (part0) を読み込み
  try {
    const content0 = fs.readFileSync(path.join(inputDir, 'data', 'tweets.js'), 'utf8');
    const jsonContent0 = content0.replace(/^window\.YTD\.tweets\.part0\s*=\s*/, '').replace(/;?\s*$/, '');
    const tweets0 = JSON.parse(jsonContent0);
    console.log(`tweets.js: ${tweets0.length}件のツイートを読み込み`);
    allTweets = allTweets.concat(tweets0);
  } catch (error) {
    console.error('tweets.jsの読み込みエラー:', error);
  }
  
  // tweets-part1.js (part1) を読み込み
  try {
    const content1 = fs.readFileSync(path.join(inputDir, 'data', 'tweets-part1.js'), 'utf8');
    const jsonContent1 = content1.replace(/^window\.YTD\.tweets\.part1\s*=\s*/, '').replace(/;?\s*$/, '');
    const tweets1 = JSON.parse(jsonContent1);
    console.log(`tweets-part1.js: ${tweets1.length}件のツイートを読み込み`);
    allTweets = allTweets.concat(tweets1);
  } catch (error) {
    console.error('tweets-part1.jsの読み込みエラー:', error);
  }
  
  if (allTweets.length === 0) {
    console.error('ツイートが読み込めませんでした');
    return null;
  }
  
  console.log(`合計: ${allTweets.length}件のツイートを読み込み完了`);
  return allTweets;
}

// ツイートを年月別に分類
function categorizeTweetsByMonth(tweets) {
  const categorized = {};
  
  console.log(`${tweets.length}件のツイートを分類中...`);
  
  tweets.forEach((tweetWrapper, index) => {
    if (index % 1000 === 0) {
      console.log(`処理中: ${index}/${tweets.length}`);
    }
    
    const tweet = tweetWrapper.tweet;
    if (tweet && tweet.created_at) {
      const { year, month } = getYearMonth(tweet.created_at);
      const key = `${year}/${month}`;
      
      if (!categorized[key]) {
        categorized[key] = [];
      }
      
      categorized[key].push(tweetWrapper);
    }
  });
  
  return categorized;
}

// 年月別のJSONファイルを保存
function saveCategorizedTweets(categorizedTweets, outputDir) {
  // すべての年を取得
  const years = new Set();
  Object.keys(categorizedTweets).forEach(yearMonth => {
    const [year] = yearMonth.split('/');
    years.add(parseInt(year));
  });
  
  // 各年について、1-12月のファイルを作成
  Array.from(years).sort().forEach(year => {
    const yearDir = path.join(outputDir, year.toString());
    ensureDirectoryExists(yearDir);
    
    for (let month = 1; month <= 12; month++) {
      const monthStr = String(month).padStart(2, '0');
      const yearMonth = `${year}/${monthStr}`;
      const filename = `${monthStr}.json`;
      const filepath = path.join(yearDir, filename);
      
      const tweets = categorizedTweets[yearMonth] || [];
      
      if (tweets.length > 0) {
        console.log(`${yearMonth}: ${tweets.length}件のツイートを ${filepath} に保存`);
      } else {
        console.log(`${yearMonth}: ツイートなし - 空のファイルを ${filepath} に作成`);
      }
      
      fs.writeFileSync(filepath, JSON.stringify(tweets, null, 2), 'utf8');
    }
  });
}

// メディアファイルを年月別ディレクトリにコピー（ツイートデータとの整合性を確認）
function copyMediaFilesByMonth(inputDir, outputDir) {
  const mediaDir = path.join(inputDir, 'data', 'tweets_media');
  
  console.log('\nメディアファイルのコピーを開始します...');
  
  if (!fs.existsSync(mediaDir)) {
    console.error(`エラー: ${mediaDir} が存在しません`);
    return;
  }
  
  // メディアIDマップを構築
  const mediaIdMap = buildMediaIdToDateMap(inputDir, outputDir);
  
  // tweets_media ディレクトリ内のファイルを取得
  const mediaFiles = fs.readdirSync(mediaDir)
    .filter(filename => fs.statSync(path.join(mediaDir, filename)).isFile());
  
  console.log(`${mediaFiles.length}個のメディアファイルを処理中...`);
  
  let successCount = 0;
  let failureCount = 0;
  let notFoundCount = 0;
  const yearMonthStats = {};
  
  mediaFiles.forEach((filename, index) => {
    if (index % 100 === 0) {
      console.log(`進行状況: ${index}/${mediaFiles.length}`);
    }
    
    const mediaId = extractMediaId(filename);
    if (!mediaId) {
      console.log(`警告: ファイル名からメディアIDを抽出できません: ${filename}`);
      failureCount++;
      return;
    }
    
    const yearMonth = mediaIdMap[mediaId];
    if (!yearMonth) {
      // Snowflake IDベースでのフォールバック処理
      const snowflakeId = extractSnowflakeFromFilename(filename);
      if (snowflakeId) {
        try {
          const date = getDateFromSnowflakeId(snowflakeId);
          const year = date.getFullYear();
          const month = String(date.getMonth() + 1).padStart(2, '0');
          const fallbackYearMonth = `${year}/${month}`;
          
          console.log(`警告: メディアID ${mediaId} (${filename}) に対応するツイートが見つかりません。Snowflake IDベースで ${fallbackYearMonth} に配置します`);
          
          const [year2, month2] = fallbackYearMonth.split('/');
          const targetDir = path.join(outputDir, year2, 'media');
          const sourcePath = path.join(mediaDir, filename);
          
          if (moveMediaFile(sourcePath, targetDir, filename)) {
            successCount++;
            yearMonthStats[fallbackYearMonth] = (yearMonthStats[fallbackYearMonth] || 0) + 1;
          } else {
            failureCount++;
          }
        } catch (error) {
          console.log(`警告: ${filename} - Snowflake ID処理エラー: ${error.message}`);
          notFoundCount++;
        }
      } else {
        notFoundCount++;
      }
      return;
    }
    
    const [year, month] = yearMonth.split('/');
    const targetDir = path.join(outputDir, year, 'media');
    const sourcePath = path.join(mediaDir, filename);
    
    if (moveMediaFile(sourcePath, targetDir, filename)) {
      successCount++;
      yearMonthStats[yearMonth] = (yearMonthStats[yearMonth] || 0) + 1;
    } else {
      failureCount++;
    }
  });
  
  console.log(`\nメディアファイルコピー完了: ${successCount}件`);
  console.log(`対応するツイートが見つからない: ${notFoundCount}件`);
  console.log(`エラー: ${failureCount}件`);
  
  // 年月別の統計を表示
  console.log('\nメディアファイル統計:');
  Object.keys(yearMonthStats).sort().forEach(yearMonth => {
    const [year, month] = yearMonth.split('/');
    console.log(`${year}年${month}月: ${yearMonthStats[yearMonth]}件`);
  });
}

// 全ての年月のJSONファイルからメディアIDマップを作成（distribute_media.jsから）
function buildMediaIdToDateMap(inputDir, outputDir) {
  const mediaIdMap = {};
  const dataDir = path.join(outputDir);  // 出力先のディレクトリから年月別JSONファイルを読み込み
  
  console.log('メディアIDマップを構築中...');
  
  // 年ディレクトリを取得
  const years = fs.readdirSync(dataDir)
    .filter(item => {
      const fullPath = path.join(dataDir, item);
      return fs.statSync(fullPath).isDirectory() && /^\d{4}$/.test(item);
    })
    .sort();
  
  let processedFiles = 0;
  const totalFiles = years.length * 12;
  
  years.forEach(year => {
    for (let month = 1; month <= 12; month++) {
      const monthStr = String(month).padStart(2, '0');
      processedFiles++;
      
      if (processedFiles % 12 === 0) {
        console.log(`進行状況: ${processedFiles}/${totalFiles} (${year}年完了)`);
      }
      
      // 出力先のJSONファイルから読み込み
      const filepath = path.join(dataDir, year, `${monthStr}.json`);
      let tweets = [];
      
      if (fs.existsSync(filepath)) {
        try {
          const content = fs.readFileSync(filepath, 'utf8');
          tweets = JSON.parse(content);
        } catch (error) {
          console.error(`エラー: ${filepath} の読み込みに失敗:`, error);
          continue;
        }
      }
      
      tweets.forEach(tweetWrapper => {
        const tweet = tweetWrapper.tweet;
        if (tweet && tweet.entities && tweet.entities.media) {
          tweet.entities.media.forEach(media => {
            if (media.media_url) {
              // media_url から メディアID を抽出
              // 例: "http://pbs.twimg.com/media/Ay8TXz8CEAAQykT.jpg" -> "Ay8TXz8CEAAQykT"
              const urlMatch = media.media_url.match(/\/media\/([^.]+)\./);
              if (urlMatch) {
                const mediaId = urlMatch[1];
                mediaIdMap[mediaId] = `${year}/${monthStr}`;
              }
            }
          });
        }
        
        // extended_entities も確認
        if (tweet && tweet.extended_entities && tweet.extended_entities.media) {
          tweet.extended_entities.media.forEach(media => {
            if (media.media_url) {
              const urlMatch = media.media_url.match(/\/media\/([^.]+)\./);
              if (urlMatch) {
                const mediaId = urlMatch[1];
                mediaIdMap[mediaId] = `${year}/${monthStr}`;
              }
            }
          });
        }
      });
    }
  });
  
  console.log(`メディアIDマップ構築完了: ${Object.keys(mediaIdMap).length}個のメディアID`);
  return mediaIdMap;
}

// メディアファイルを移動（distribute_media.jsから）
function moveMediaFile(sourcePath, targetDir, filename) {
  ensureDirectoryExists(targetDir);
  const targetPath = path.join(targetDir, filename);
  
  try {
    // ファイルをコピー（元ファイルは保持）
    fs.copyFileSync(sourcePath, targetPath);
    return true;
  } catch (error) {
    console.error(`ファイル移動エラー: ${sourcePath} -> ${targetPath}:`, error);
    return false;
  }
}

// メイン処理
function main() {
  // 基準ディレクトリを取得
  const { inputDir, outputDir } = getDirectories();
  
  console.log('Twitter archive の分割処理を開始します...');
  
  try {
    // 1. ツイートデータの分割処理
    console.log('\n=== ツイートデータの分割処理 ===');
    
    // ツイートを読み込み
    const tweets = loadTweets(inputDir);
    if (!tweets) {
      console.error('ツイートの読み込みに失敗しました');
      return;
    }
    
    // 年月別に分類
    const categorizedTweets = categorizeTweetsByMonth(tweets);
    
    // ファイルに保存
    saveCategorizedTweets(categorizedTweets, outputDir);
    
    console.log('ツイート分割処理が完了しました！');
    console.log('分割されたファイル:');
    
    // すべての年を取得してソート
    const years = new Set();
    Object.keys(categorizedTweets).forEach(yearMonth => {
      const [year] = yearMonth.split('/');
      years.add(parseInt(year));
    });
    
    Array.from(years).sort().forEach(year => {
      console.log(`\n${year}年:`);
      for (let month = 1; month <= 12; month++) {
        const monthStr = String(month).padStart(2, '0');
        const yearMonth = `${year}/${monthStr}`;
        const count = categorizedTweets[yearMonth] ? categorizedTweets[yearMonth].length : 0;
        console.log(`  ${monthStr}月: ${count}件`);
      }
    });
    
    // 2. メディアファイルの分割処理
    console.log('\n=== メディアファイルの分割処理（ツイートデータ連携方式） ===');
    
    // メディアファイルをツイートデータとの整合性を確認してコピー
    copyMediaFilesByMonth(inputDir, outputDir);
    console.log('メディアファイル分割処理が完了しました！');
    
    console.log('\n全ての処理が完了しました！');
    
  } catch (error) {
    console.error('エラーが発生しました:', error);
  }
}

main();
