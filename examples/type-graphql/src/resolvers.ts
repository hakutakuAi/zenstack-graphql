import { Resolver, Query, Mutation, Arg, Ctx, FieldResolver, Root } from 'type-graphql'
import { PrismaClient } from '@prisma/client'
import { User, Post, Category, Comment } from '../schema'

export interface Context {
	prisma: PrismaClient
}

@Resolver(() => User)
export class UserResolver {
	@Query(() => [User])
	async users(@Ctx() { prisma }: Context): Promise<User[]> {
		return (await prisma.user.findMany()) as User[]
	}

	@Query(() => User, { nullable: true })
	async user(@Arg('id', () => String) id: string, @Ctx() { prisma }: Context): Promise<User | null> {
		return (await prisma.user.findUnique({
			where: { id },
		})) as User | null
	}

	@Mutation(() => User)
	async createUser(
		@Arg('name', () => String) name: string,
		@Arg('email', () => String) email: string,
		@Arg('bio', () => String, { nullable: true }) bio: string | undefined,
		@Ctx() { prisma }: Context,
	): Promise<User> {
		return (await prisma.user.create({
			data: {
				name,
				email,
				bio,
			},
		})) as User
	}

	@FieldResolver(() => [Post])
	async posts(@Root() user: User, @Ctx() { prisma }: Context): Promise<Post[]> {
		return await prisma.post.findMany({
			where: { authorId: user.id },
		})
	}

	@FieldResolver(() => [Comment])
	async comments(@Root() user: User, @Ctx() { prisma }: Context): Promise<Comment[]> {
		return await prisma.comment.findMany({
			where: { authorId: user.id },
		})
	}
}

@Resolver(() => Post)
export class PostResolver {
	@Query(() => [Post])
	async posts(@Ctx() { prisma }: Context): Promise<Post[]> {
		return await prisma.post.findMany()
	}

	@Query(() => Post, { nullable: true })
	async post(@Arg('id', () => String) id: string, @Ctx() { prisma }: Context): Promise<Post | null> {
		return await prisma.post.findUnique({
			where: { id },
		})
	}

	@Mutation(() => Post)
	async createPost(
		@Arg('title', () => String) title: string,
		@Arg('content', () => String) content: string,
		@Arg('authorId', () => String) authorId: string,
		@Arg('published', () => Boolean, { defaultValue: false }) published: boolean = false,
		@Ctx() { prisma }: Context,
	): Promise<Post> {
		return await prisma.post.create({
			data: {
				title,
				content,
				published,
				authorId,
			},
		})
	}

	@Mutation(() => Post, { nullable: true })
	async publishPost(@Arg('id', () => String) id: string, @Ctx() { prisma }: Context): Promise<Post | null> {
		return await prisma.post.update({
			where: { id },
			data: { published: true },
		})
	}

	@FieldResolver(() => User)
	async author(@Root() post: Post, @Ctx() { prisma }: Context): Promise<User | null> {
		return (await prisma.user.findUnique({
			where: { id: post.authorId },
		})) as User | null
	}

	@FieldResolver(() => [Category])
	async categories(@Root() post: Post, @Ctx() { prisma }: Context): Promise<Category[]> {
		const categoryOnPosts = await prisma.categoryOnPost.findMany({
			where: { postId: post.id },
			include: { category: true },
		})
		return categoryOnPosts.map((cop: any) => cop.category as Category)
	}

	@FieldResolver(() => [Comment])
	async comments(@Root() post: Post, @Ctx() { prisma }: Context): Promise<Comment[]> {
		return await prisma.comment.findMany({
			where: { postId: post.id },
		})
	}
}

@Resolver(() => Category)
export class CategoryResolver {
	@Query(() => [Category])
	async categories(@Ctx() { prisma }: Context): Promise<Category[]> {
		return (await prisma.category.findMany()) as Category[]
	}

	@Query(() => Category, { nullable: true })
	async category(@Arg('id', () => String) id: string, @Ctx() { prisma }: Context): Promise<Category | null> {
		return (await prisma.category.findUnique({
			where: { id },
		})) as Category | null
	}

	@Mutation(() => Category)
	async createCategory(
		@Arg('name', () => String) name: string,
		@Arg('description', () => String, { nullable: true }) description: string | undefined,
		@Ctx() { prisma }: Context,
	): Promise<Category> {
		return (await prisma.category.create({
			data: {
				name,
				description,
			},
		})) as Category
	}

	@FieldResolver(() => [Post])
	async posts(@Root() category: Category, @Ctx() { prisma }: Context): Promise<Post[]> {
		const categoryOnPosts = await prisma.categoryOnPost.findMany({
			where: { categoryId: category.id },
			include: { post: true },
		})
		return categoryOnPosts.map((cop: any) => cop.post as Post)
	}
}

@Resolver(() => Comment)
export class CommentResolver {
	@Query(() => [Comment])
	async comments(@Ctx() { prisma }: Context): Promise<Comment[]> {
		return await prisma.comment.findMany()
	}

	@Mutation(() => Comment)
	async createComment(
		@Arg('content', () => String) content: string,
		@Arg('postId', () => String) postId: string,
		@Arg('authorId', () => String) authorId: string,
		@Ctx() { prisma }: Context,
	): Promise<Comment> {
		return await prisma.comment.create({
			data: {
				content,
				postId,
				authorId,
			},
		})
	}

	@FieldResolver(() => User)
	async author(@Root() comment: Comment, @Ctx() { prisma }: Context): Promise<User | null> {
		return (await prisma.user.findUnique({
			where: { id: comment.authorId },
		})) as User | null
	}

	@FieldResolver(() => Post)
	async post(@Root() comment: Comment, @Ctx() { prisma }: Context): Promise<Post | null> {
		return await prisma.post.findUnique({
			where: { id: comment.postId },
		})
	}
}
