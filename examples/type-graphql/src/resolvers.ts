import { Resolver, Query, Mutation, Arg, ID } from 'type-graphql'
import { User, Post } from '../schema'

@Resolver()
export class UserResolver {
	private users: User[] = [
		{
			id: '1',
			createdAt: new Date(),
			updatedAt: new Date(),
			name: 'John Doe',
			email: 'john@example.com',
			bio: 'Software developer',
		},
		{
			id: '2',
			createdAt: new Date(),
			updatedAt: new Date(),
			name: 'Jane Smith',
			email: 'jane@example.com',
			bio: 'Designer',
		},
	]

	@Query(() => [User])
	getUsers(): User[] {
		return this.users
	}

	@Query(() => User, { nullable: true })
	getUser(@Arg('id', () => ID) id: string): User | undefined {
		return this.users.find((user) => user.id === id)
	}

	@Mutation(() => User)
	createUser(
		@Arg('name', () => String) name: string,
		@Arg('email', () => String) email: string,
		@Arg('bio', () => String, { nullable: true }) bio?: string,
	): User {
		const newUser: User = {
			id: (this.users.length + 1).toString(),
			createdAt: new Date(),
			updatedAt: new Date(),
			name,
			email,
			bio,
		}
		this.users.push(newUser)
		return newUser
	}
}

@Resolver()
export class PostResolver {
	private posts: Post[] = [
		{
			id: '1',
			createdAt: new Date(),
			updatedAt: new Date(),
			title: 'First Post',
			content: 'This is the first post content',
			published: true,
			authorId: '1',
		},
		{
			id: '2',
			createdAt: new Date(),
			updatedAt: new Date(),
			title: 'Second Post',
			content: 'This is the second post content',
			published: false,
			authorId: '2',
		},
	]

	@Query(() => [Post])
	getPosts(): Post[] {
		return this.posts
	}

	@Query(() => Post, { nullable: true })
	getPost(@Arg('id', () => ID) id: string): Post | undefined {
		return this.posts.find((post) => post.id === id)
	}

	@Mutation(() => Post)
	createPost(
		@Arg('title', () => String) title: string,
		@Arg('content', () => String) content: string,
		@Arg('authorId', () => String) authorId: string,
		@Arg('published', () => Boolean, { defaultValue: false }) published: boolean = false,
	): Post {
		const newPost: Post = {
			id: (this.posts.length + 1).toString(),
			createdAt: new Date(),
			updatedAt: new Date(),
			title,
			content,
			published,
			authorId,
		}
		this.posts.push(newPost)
		return newPost
	}

	@Mutation(() => Post, { nullable: true })
	publishPost(@Arg('id', () => ID) id: string): Post | undefined {
		const post = this.posts.find((p) => p.id === id)
		if (post) {
			post.published = true
			post.updatedAt = new Date()
		}
		return post
	}
}
