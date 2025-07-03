class Commit
  attr_reader :sha
end

class Committer
  attr_reader :name, :email, :github_user, :commits

  def initialize(name:, email:, github_user:)
    @name = name
    @email = email
    @github_user = github_user
    @commits = []
  end

  def each_commit(&block)
    if block
      commits.each(&block)
    else
      enum_for :each_commit
    end
  end
end

committer = Committer.new(name: :soutaro, email: nil, github_user: "soutaro")
